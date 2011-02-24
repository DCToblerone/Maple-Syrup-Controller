;----------------------------------------------------------------------
; lcd.asm
;
; ***** BEGIN LICENSE BLOCK *****
; Version: MPL 1.1
;
; The contents of this file are subject to the Mozilla Public License Version 
; 1.1 (the "License"); you may not use this file except in compliance with 
; the License. You may obtain a copy of the License at 
; http://www.mozilla.org/MPL/
;
; Software distributed under the License is distributed on an "AS IS" basis,
; WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
; for the specific language governing rights and limitations under the
; License.
;
; The Original Code is the LCD interface module code.
;
; The Initial Developer of the Ori-ginal Code is David M. Clay.
; Portions created by the Initial Developer are 
; Copyright (C) 2010-2011 Daivd M. Clay (the Initial Developer).
; All Rights Reserved.
;
;Contributor(s):
;	David Clay <dave!at!claysrus!dot!com>
;
;***** END LICENSE BLOCK *****
;----------------------------------------------------------------------
	include <p16f88.inc>
	include "isr.inc"
	include "math.inc"
	radix dec

	global	LCDInit
	global	LCDGoto
	global	LCDPutChar
	global	LCDPutCStr
	global	LCDPutStr
	global	LCDClear
	global	LCDPutUInt
	global	LCDPutFpDX
	global	LCDPutHex

; LCD lines on PORTB
; LCD_D7:LCD_D4 must be on upper nybble of PORTB
LCD_RS	EQU	1	; Register select
LCD_RW	EQU	2	; Readwrite control
LCD_EN	EQU	3	; Enable
LCD_D4	EQU	4	; Data bits
LCD_D5	EQU	5	; Data bits
LCD_D6	EQU	6	; Data bits
LCD_D7	EQU	7	; Data bits

FUNC_CLEAR	EQU	0x01
FUNC_HOME	EQU	0x02
FUNC_MODE	EQU	0x04
FUNC_DISP	EQU	0x08
FUNC_CRSR	EQU	0x10
FUNC_FUNC	EQU	0x20
FUNC_CGRA	EQU	0x40
FUNC_GOTO	EQU	0x80 ; 1-line: 0x00 - 0x4f, 2-line: 0x00-0x27 and 0x40-0x67

LCD_STROBE	MACRO
	BSF	PORTB,LCD_EN
	BCF	PORTB,LCD_EN
 ENDM

_DATA UDATA
RBSave	res	1
lcd_wsave	res 1
lcd_BF	res	1
sav_RS	res	1

LCD	code

LCDMode
	movwf	lcd_wsave
	BCF 	INTCON, RBIE
	movf	PORTB,w
	movwf	RBSave
; next 4 lines now handled in LCDWait
;	BANKSEL	TRISB
;	CLRF	TRISB	; All are outputs. All are 0 except LCD_RS, which should be 1
;	BANKSEL	PORTB
;	CLRF	PORTB
	movf	lcd_wsave,w
	RETURN

LCDDone
	BCF 	RBSave, LCD_EN	; make sure this is clear
	movf	RBSave, w
	movwf	PORTB
	; make data lines input, others output!
	BANKSEL	TRISB
	movlw	((1<<LCD_D7)|(1<<LCD_D6)|(1<<LCD_D5)|(1<<LCD_D4))
	MOVWF	TRISB
	BANKSEL	PORTB
	BSF 	INTCON, RBIE
	RETURN

; check busy flag, and wait if needed
LCDWait
	; make data lines input, others output!
	BANKSEL	TRISB
	movlw	((1<<LCD_D7)|(1<<LCD_D6)|(1<<LCD_D5)|(1<<LCD_D4))
	movwf	TRISB
	BANKSEL	PORTB
	BCF	PORTB,LCD_RS

LCDbusyloop
	BSF	PORTB,LCD_RW	 ; READ!
	BSF	PORTB,LCD_EN 	; Enable
	movf	PORTB, w	; Get BF, AC6:4
	movwf	lcd_BF	; save so we can check later
	BCF	PORTB,LCD_EN	; Disable
	BSF	PORTB,LCD_EN	; Enable
	; AC3:0 address counter is available now, but do we need it? No.
	BCF	PORTB,LCD_EN	; Disable
	BCF	PORTB,LCD_RW	; Go back to WRITE, so that we can check buttons or set PORTB to outputs
	BTFSC	lcd_BF,LCD_D7	; are we still busy?
;	BTFSS	lcd_BF,LCD_D7	; are we still busy?
;	GOTO	LCDready	; nope, get out!

	; don't go too long w/o allowing button interrup to run
;	BSF 	INTCON, RBIE	; Turn interrupt back on
;	DelayUs	6
;	BCF 	INTCON, RBIE	; Turn interrupt back off
	GOTO	LCDbusyloop
;LCDready

	; make data lines output again
	BANKSEL	TRISB
	clrf	TRISB
	BANKSEL	PORTB
	CLRF	PORTB
	BTFSC	sav_RS, LCD_RS
	BSF 	PORTB, LCD_RS
	RETURN


LCDInit
	call	LCDMode
	; can't call LCDWait so we set to ouput and clear manually
	BANKSEL	TRISB
	CLRF	TRISB	; All are outputs. All are 0.
	BANKSEL	PORTB
	CLRF	PORTB
	CLRF	sav_RS	; we want LCD_RS set to 0 for all of these!
	
	MOVLW	30	; wait 30ms for power to stabilize on the LCD module
	CALL	DelayMs

	; init 00110000
	BSF	PORTB,LCD_D4
	BSF	PORTB,LCD_D5
	LCD_STROBE
	MOVLW	6	; DMC: HD44780 says to "wait for more than 4.1ms"
	CALL	DelayMs
	LCD_STROBE
	DelayUs 200	; DMC: HD44780 says to "wait for more than 100us"
	LCD_STROBE ; init again
	
	MOVLW	10	; wait 10ms
	CALL	DelayMs

	; 4-bit mode
	BCF	PORTB,LCD_D4
	LCD_STROBE
	DelayUs 80	
	BCF	PORTB,LCD_D5
	; DMC: LCD_D7:4 should be 0000 and this is the default state!

	; lcd_write(FUNC_FUNC | 0x08);// DL, N, F, *, * = 01000 4 bit mode, 2 lines, 5x8 font 1/16 duty
	MOVLW	(FUNC_FUNC | 0x08)
	CALL	LCDWrite

	; lcd_write(FUNC_DISP | 0x04);// D, C, B = 100 display on, cursor off, blinking off
	MOVLW	(FUNC_DISP | 0x04)
	CALL	LCDWrite

	; lcd_write(FUNC_MODE | 0x02);// I/D, S = 10 increment cursor, shift off
	MOVLW	(FUNC_MODE | 0x02)
	CALL	LCDWrite

	; lcd_write(FUNC_CLEAR);// clear display and reset cursor (sets FUNC_MODE to 10)
	MOVLW	FUNC_CLEAR
	CALL	LCDWrite

	goto	LCDDone

LCDGoto
	call	LCDMode
	BCF 	sav_RS, LCD_RS
	IORLW	FUNC_GOTO
	CALL	LCDWrite
	goto	LCDDone

LCDPutChar
	call	LCDMode
	BSF 	sav_RS, LCD_RS
	CALL	LCDWrite
	goto	LCDDone

LCDPutHex
	call	LCDMode
	BSF 	sav_RS, LCD_RS
	SWAPF	AH, w
	CALL	HexDigit
	CALL	LCDWrite
	MOVF	AH, w
	CALL	HexDigit
	CALL	LCDWrite
	SWAPF	AL, w
	CALL	HexDigit
	CALL	LCDWrite
	MOVF	AL, w
	CALL	HexDigit
	CALL	LCDWrite
	goto	LCDDone


LCDPutCStr
	call	LCDMode
	BSF 	sav_RS, LCD_RS
	movwf	FSR
next:
	call	nxtchr
	iorlw	0
	btfsc	STATUS, Z
	goto	LCDDone	; returns from this func!
	call	LCDWrite
	goto	next
	
LCDPutStr
	call	LCDMode
	BSF 	sav_RS, LCD_RS
	movwf	FSR
nextps:
	movf	INDF,w
	incfsz	FSR,f ; does NOT modify STATUS_bits.Z flag
	btfsc	STATUS, Z
	goto	LCDDone	; returns from this func
	call	LCDWrite
	goto	nextps

LCDClear
	call	LCDMode
	BCF 	sav_RS, LCD_RS
	MOVLW	FUNC_CLEAR
	CALL	LCDWrite
	goto	LCDDone

; output fixed-point number in DU:DH.DL
LCDPutFpDX
	MOVWORD	AX,DX+1
	CALL	LCDPutUInt	; kills AX et al.
	movlw	'.'
	CALL	LCDPutChar
	CLRF	CH	; no padding
;	BSF 	CH,2	; do two-digit zeros
	movf	DX,w
	;	AX = DL * 100 / 256
	movwf	AX
	clrf	AX+1
	movlw	d'10'
	movwf	BX
	clrf	BX+1
	CALL	Mult16x16	; PROD = AX * BX
	MOVWORD	AX,PROD+1
	; -- now AX = DL * 100 / 256
	goto	LCDPutUInt	; kills AX et al.
	; add a space to be sure a shrinking number doesn't leave trails
;	movlw	' '
;	GOTO	LCDPutChar	; returns
;	CALL	LCDPutChar
;	RETURN

; Write unsigned AX to the LCD
; Set CH bit 0 to pad, clear to not pad
; Set CH bit 1 to pad with 0s, clear to pad with spaces
; Set CH bit 2 for clock padding (two digits)
LCDPutUInt	;Function start
	call	LCDMode
	BSF 	sav_RS, LCD_RS
	CALL	Bin2BCD	; convert AX to BCDU,BCDH,BCDL
	BCF 	CH,7	; clear the "already seen first non-zero digit" flag
;	SWAPF	BCDU, w
;	CALL	LCDWrite0
	MOVF	BCDU, w
	CALL	LCDWrite0
	SWAPF	BCDH, w
	CALL	LCDWrite0
	MOVF	BCDH, w
	CALL	LCDWrite0
	BTFSC	CH,2	; should we turn on padding before last two digits?
	BSF	CH,7	; Set last digit early so we always get a 00
	SWAPF	BCDL, w
	CALL	LCDWrite0
	BSF	CH,7	; Set last digit so we always get a 0
	MOVF	BCDL, w
	CALL	LCDWrite0
	goto	LCDDone

; Assumes that LCDMode has already been called!
LCDWrite0
	ANDLW	0x0f
	BTFSS	CH,7	; already seen first non-zero digit? skip straight to output
	BTFSS	STATUS,Z
	goto	wr0_nonzero	; w>0 so just output
	BTFSS	CH,0	; CH:0 set means to pad 
	RETURN
	BTFSC	CH,1
	GOTO	wr0_zero
	movlw	0x20	; w=0, so set to space
	GOTO	LCDWrite
wr0_nonzero
	BSF 	CH,7	; set the 'seen first non-zero digit' flag
wr0_zero
	addlw	0x30	; start at'0'
;	GOTO	LCDWrite
	; just fall down into LCDWrite

; Assumes that LCDMode has already been called!
LCDWrite	;Function start
	MOVWF	CL
	CALL	LCDWait	; Is controller busy?
	MOVF	CL,w	; get input back!
	; DMC: RB7:4 should be 0000 and this is the default state!
	ANDLW	0xf0
	IORWF	PORTB, F
	LCD_STROBE	; keeps W
	XORWF	PORTB, F	; clears RB7:4
	SWAPF	CL,W
	ANDLW	0xf0
	IORWF	PORTB, F
	LCD_STROBE	; keeps W
	XORWF	PORTB, F	; clears RB7:4
	; DMC: HD44780 says that max execute time is 37us!
	; DMC: but now we don't have to delay at all since we check the
	; DMC: busy flag from the controller in LCDWait!
	RETURN

END
