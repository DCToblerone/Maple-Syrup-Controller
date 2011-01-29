;----------------------------------------------------------------------
; maple.asm
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
; The Original Code is the main program for the maple syrup controller.
;
; The Initial Developer of the Original Code is David M. Clay.
; Portions created by the Initial Developer are 
; Copyright (C) 2010-2011 Daivd M. Clay (the Initial Developer).
; All Rights Reserved.
;
;Contributor(s):
;	David Clay <dave!at!claysrus!dot!com>
;
;***** END LICENSE BLOCK *****
;----------------------------------------------------------------------

    #include <p16f88.inc>
	radix dec
	#include "isr.inc"
	#include "sched.inc"
	#include "lcd.inc"
	#include "math.inc"

; -----------------------------------------------------------------------
; Configuration bits: adapt to your setup and needs
;	__CONFIG _HS_OSC & _WDT_OFF & _PWRTE_OFF & _CP_OFF
	__CONFIG	_CONFIG1, _CP_OFF & _CCP1_RB0 & _DEBUG_OFF & _WRT_PROTECT_OFF & _CPD_OFF & _LVP_OFF & _BODEN_OFF & _MCLR_OFF & _PWRTE_OFF & _WDT_OFF & _EXTCLK
	__CONFIG	_CONFIG2, _IESO_OFF & _FCMEN_OFF


	global	clock

; -----------------------------------------------------------------------
; Variables declaration
_EEPROM CODE 0x2100
tGals	dw	0x00
tGalsH	dw	0x00
tMins	dw	0x00
tMinsH	dw	0x00


_DATA UDATA
eSec	res	2
clk	res	3
rCount	RES 3
;tCount	RES 2
ntmp	RES 1
mrf_bit	RES	1
mrf_max RES 1
alarm_tmr RES 1
pump_flags	RES 1
pfPumping	EQU	0
pfError	EQU	1

;tPos	RES	1

; set out timeout value to 7.5 miliseconds, this ought to be
; ample time for the capacitor to discharge
; DMC: Make this time unusually large so we can run the button/menu code more
#define DischargeTime 75000
#define ButtonTime	25000

; -----------------------------------------------------------------------
; reset vector
STARTUP CODE 0x000
    nop                    ; needed for ICD2 debugging
    movlw   high start     ; load upper byte of 'start' label
    movwf   PCLATH         ; initialize PCLATH
    goto    start          ; go to start of main code


; relocatable code
PROG CODE

str_Hello	DT "  Maple Controller",0
str_Gal		DT " gal",0

start
	BANKSEL 0
	clrf	PORTA	; turn off pump et al.
	BANKSEL	ANSEL
	clrf	ANSEL	; configuer all pins as digital (not analog)
;	BANKSEL	OPTION_REG
	; OPTION_REG = 0xc0 | TMR0Prescaler
	; 01000<000:TMR0Prescaler> PORTB pullups on, INTEDG rising, T0 to clock, Prescaler assigned to TMR0
ifdef TMR0Prescaler
	MOVLW	(0x40 | TMR0Prescaler)
else ; assign prescaler to WDT
	MOVLW	0x48
endif
	MOVWF	OPTION_REG

;	BANKSEL	TRISA	; this should already be set
	MOVLW	((1<<2)|(1<<1)|(1<<0)) ; set our measuring pins to inputs!
	MOVWF	TRISA
	CLRF	TRISB	; RB0 and the rest are outputs (we use RB0 to discharge the timing capacitor!)
	BANKSEL	0
	clrf	PORTB	; clear RB0 to discharge capacitor
;	bcf 	PORTA, 4	; turn off pump
	bsf 	PORTA, 3	; turn off buzzer

	CLRF	nButts
	MOVLW	0x70	; default to buttons up
	MOVWF	Butts

	; INTCON = 0xa0;	// 10100000	Enable T0 interrupt
	MOVLW	(1<<GIE)|(1<<TMR0IE)|(1<<RBIE)
	MOVWF	INTCON
	CALL	LCDInit
	CALL	LCDClear
	movlw high(str_Hello)
	movwf PCLATH
	movlw low(str_Hello)
	call LCDPutCStr
	
	CLRF	clk
	CLRF	clk+1
	CLRF	clk+2
	CLRF	eSec
	; thread 0 will call the clock routine once every second
	Schedule	0,1,1000000,20000,clock

	; thread 1 will simply clear its flag after timeout
	Schedule	1,1,DischargeTime,100000,Thread1

	; thread 2 will call the button routines often!
	Schedule	2,1,ButtonTime,20000,btnThread

Thread1

	movlw	(1<<0)	; RA0
	call	MeasureResistance4
	
;	SETWORDLIT	rCount,0x2fd0

	MOVLW	d'0'
	CALL	LCDGoto
	MOVWORD	AX,rCount
	clrf	CH
	BSF	CH,0	; pad
	CALL	LCDPutUInt	; kills AX et al.

	MOVLW	d'84'
;	MOVWF	tPos
	CALL	LCDGoto	; kills AX

	MOVWORD	BX,rCount
	movlw	high(P0Spline)
	movwf	PCLATH
	movlw	P0Spline
	call	InterpolateSpline
	call	LCDPutFpDX

	movlw	(1<<1)	; RA1
	call	MeasureResistance4

	MOVLW	d'7'
	CALL	LCDGoto	; kills AX
	MOVWORD	AX,rCount
	clrf	CH
	BSF	CH,0	; pad
;	CALL	LCDPutHex
	CALL	LCDPutUInt	; kills AX et al.

	MOVLW	d'91'
;	MOVWF	tPos
	CALL	LCDGoto	; kills AX

	MOVWORD	BX,rCount
	movlw	high(P1Spline)
	movwf	PCLATH
	movlw	P1Spline
	call	InterpolateSpline
	call	LCDPutFpDX


CheckPump
	clrf	rCount+1
	clrf	rCount
	movlw	(1<<2)	; RA2
	call	MeasureResistance

	MOVLW	d'98'
	CALL	LCDGoto	; kills AX
	clrf	CH
	BSF	CH,0	; pad
	MOVWORD	AX,rCount
	CALL	LCDPutUInt	; kills AX et al.

	movf	rCount+1,w
	sublw	0x10
	BTFSC	STATUS,C
	GOTO	CheckFull	; check if we are full
	; Carry clear, so result is negative and rCount+1 > 0x11
	; no sap is being detected so turn on pump
	btfsc	PORTA, 4	; is the pump already on?
	goto	PumpStay	; yes, stay and check if we should alarm
	bcf 	pump_flags, pfPumping	; clear this until we detect actual pumping
	movlw	7	; set alarm timer
	movwf	alarm_tmr
	bsf 	PORTA, 4	; turn the pump ON
	GOTO	PumpDone

CheckFull	; check if we are full
	movf	rCount+1,w
	sublw	0x2
	BTFSC	STATUS,C
	GOTO	TankFull
	; Carry clear, so result is negative and rCount+1 > 0x02
	; tank is not full yet, so setup to alarm after 30 counts
	btfsc 	pump_flags, pfPumping	; have we detected pumping yet?
	GOTO	PumpStay	; yes we did, so stay where we are and keep counting
	bsf 	pump_flags, pfPumping	; set that we've detected pumping
	bsf 	PORTA, 3	; turn any previous alaram off
	movlw	20	; set alarm timer to 20 so we can alarm if we never fill
	movwf	alarm_tmr
	GOTO	PumpStay	; stay and count if we should alarm

TankFull
	; Carry set, so result is 0 or positive rCount+1 <= 0x02
	; this means we are full and we need to turn the pump off
	btfss	PORTA, 4	; is the pump already off?
	goto	PumpDone	; yes, then we're done
	bcf 	PORTA, 4	; turn off the pump
	bsf 	PORTA, 3	; turn any previous alaram off

	; we have pumped the calibrated amount (assume 1 gallon for now)
	; update the count and compute gal/hr
	movlw	rCount	; store into rCount
	movwf	FSR
	movlw	tGals
	CALL	EERead
	CALL	EERead
	INCFSZ	rCount,F
	BTFSC	PORTA,7	; this will always be clear, and so is a skip
	INCF	rCount+1,F

	MOVLW	d'20'
	CALL	LCDGoto	; kills AX
	clrf	CH
	BSF	CH,0	; pad
	MOVWORD	AX,rCount
	CALL	LCDPutUInt	; kills AX et al.

	movlw high(str_Gal)
	movwf PCLATH
	movlw low(str_Gal)
	call LCDPutCStr

	movlw	rCount	; store into rCount
	movwf	FSR
	movlw	tGals
	CALL	EEWrite
	CALL	EEWrite

PumpStay
	btfss	PORTA, 4	; is the pump on?
	goto	PumpDone	; no, so just get out
	decfsz	alarm_tmr,f	; should we alaram?
	GOTO	PumpDone	; not yet...
	btfss	PORTA, 3	; is the alarm already on?
	GOTO	PumpOff 	; yes, then something is majorly wrong.. turn the pump off!
	bcf 	PORTA, 3	; turn the alarm on!
	movlw	40	; set alarm timer to we can turn the pump off
	movwf	alarm_tmr
;	GOTO	PumpDone

PumpDone
	GOTO	Thread1

PumpOff
	bcf 	PORTA, 4	; turn the pump off
	bsf 	pump_flags, pfError	; need to make sure it doesnt come back on!

	GOTO	Thread1




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
MeasureResistance4
	movwf	mrf_bit
	movlw	d'4'
	movwf	ntmp
	clrf	DX
	clrf	DX+1
	clrf	DX+2
THR_refloop:
	clrf	rCount
	clrf	rCount+1
	call	mr_int
	btfsc	rCount+1, 7	; are we getting really big counts >32767?
	RETURN				; if so, just get out.. these counts take forever!
	ADD2416	DX,rCount
	decfsz	ntmp,f
	GOTO	THR_refloop
	; rCount = DX / 4
	BCF 	STATUS,C
	RRF 	DX+2,f
	RRF 	DX+1,f
	RRF 	DX,f
	BCF 	STATUS,C
	RRF 	DX+2,f
	RRF 	DX+1,f
	RRF 	DX,f
	MOVWORD	rCount,DX
	RETURN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
MeasureResistance
; we need to save w on input
	movwf	mrf_bit
	; set mrf_max so we don't allow TMR0 interrupt to be missed
    ; it could be missed if it's allowed to happen once
    ; it will be certainly be missed if it's allowed to happen twice
mr_int
	movlw	(CyclesPerTMR0H-48)/256/5
	movwf	mrf_max

;	; all measuring pins should already be inputs
;	; RB0 should be output 0, discharging the cap

	; calls mainloop and allows other threads to run while we wait
	; for the capacitor to discharge

	ScheduleWaitForTimeout	1
	
	; we should have waited at least DischargeTime microseconds at this point
	; and the capacitor should be discharged by now!

	; shouldn't have to worry about the READ/MODIFY/WRITE
	; cycle accidentally setting RA0 high since the cap
	; must be discharged right now
	movf	mrf_bit,w
	IORWF	PORTA,f ; make sure output latch high
	BCF 	INTCON,GIE	; diable all interrupts during the charge
	BANKSEL	TRISB
	BSF 	OPTION_REG, NOT_RBPU	; turn pull ups OFF!
	BSF	TRISB,0	; make RB0 an input
	XORWF	TRISA,F		; go to output, to charge capacitor
	BANKSEL	0
MRF_loop
		BTFSC	PORTB, 0
		GOTO	MRF_loopdone
		INCFSZ	rCount,F
		GOTO	MRF_loop
		INCF	rCount+1,F
		DECFSZ	mrf_max,F	; get out of this loop so that we don't miss TMR0 interrupt!
		GOTO	MRF_loop
		BTFSC	INTCON,GIE	; did we already enable interrupts?
		GOTO	MRF_loopdone	; if so, get out
		; we'll re-enable interrupts. It will be slightly less accurate for higher counts
		; but the overall error percentage should be tolerable (with such high counts)
		BSF	INTCON,GIE	; re-enable all interrupts so we don't miss TMR0 interrupt!
		;;; arbitrary timeout so that 0x3f00 16128 will be out max time
		;;; (since we add 4 measurements and shift twice for average)
		;;movlw	0x40-1-(CyclesPerTMR0H-32)/256/5
		movlw	0xc0-1-(CyclesPerTMR0H-48)/256/5
		movwf	mrf_max
		GOTO	MRF_loop
MRF_loopdone
	BCF	PORTB,0	; make sure RB0 is 0 so we discharge
	movf	mrf_bit,w
	BANKSEL	TRISA
	IORWF	TRISA,F		; go back to an input
	BCF	TRISB,0	; make RB0 an output so we discharge the capacitor
	BCF 	OPTION_REG, NOT_RBPU	; turn pull ups back ON!
	BANKSEL	0
	BSF	INTCON,GIE	; re-enable all interrupts after the charge
	; set out timeout value for DischargeTime miliseconds from now
	; this ought to be ample time for the capacitor to discharge
	ScheduleTime	1, DischargeTime

	RETURN


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Thread0
;
Thread0
clock	;Function start

	ScheduleAddAccurateTime	0, 1000000

	incf	eSec,f
	BTFSC	STATUS,Z
	incf	eSec+1,f
	incf	clk,w
	movwf	clk
	sublw	d'60'
	BTFSS	STATUS,Z
	GOTO	clk_out
	clrf	clk
	incf	clk+1,w
	movwf	clk+1
	sublw	d'60'
	BTFSS	STATUS,Z
	GOTO	clk_out
	clrf	clk+1
	incf	clk+2,f

clk_out:
	MOVLW	64
	CALL	LCDGoto
	clrf	CH	; no padding
	CLRF	AH
	movf	clk+2,w
	movwf	AL
	CALL	LCDPutUInt
	movlw	':'
	CALL	LCDPutChar
	BSF	CH,2	; do two-digit zeros
	CLRF	AH
	movf	clk+1,w
	movwf	AL
	CALL	LCDPutUInt
	movlw	':'
	CALL	LCDPutChar
	CLRF	AH
	movf	clk,w
	movwf	AL
	CALL	LCDPutUInt
	; call mainloop and allows other threads to run
	GOTO	mainloop


; struct line
; {
;	unsigned short knot:16
;	unsigned short slope:16
;	unsgined short intercept:16
;	17th bit of intercept is computed: starts as 1, gets cleared when (signed) intercept flips from pos to neg
; }

; Big Endian
P0Spline
;	DT	d'33'	; count
	DT	0x00,0xD9,0x65,0x72,0x5E,0x00
	DT	0x01,0x09,0x41,0x5C,0x3F,0x69
	DT	0x01,0x2A,0x39,0xB0,0x37,0x78
	DT	0x01,0x95,0x2B,0x79,0x26,0xEC
	DT	0x01,0xA0,0x1B,0xD3,0x0E,0x2A
	DT	0x01,0xB9,0x22,0x27,0x18,0x74
	DT	0x01,0xD6,0x1F,0xD1,0x14,0x6C
	DT	0x02,0x11,0x1D,0x8B,0x10,0x41
	DT	0x02,0x24,0x12,0xF4,0xFA,0x5E
	DT	0x02,0x2D,0x16,0x18,0x01,0x17
	DT	0x02,0x33,0x17,0x68,0x03,0xF1
	DT	0x02,0x3E,0x16,0xA4,0x02,0x42
	DT	0x02,0x46,0x17,0x88,0x04,0x43
	DT	0x02,0x57,0x19,0x5D,0x08,0x6D
	DT	0x02,0x6A,0x17,0xCB,0x04,0xBF
	DT	0x02,0xAF,0x14,0xD9,0xFD,0xA4
	DT	0x03,0x18,0x11,0xDB,0xF5,0x9B
	DT	0x03,0xDE,0x0E,0xFD,0xEC,0xBF
	DT	0x04,0xD2,0x0B,0x89,0xDF,0x63
	DT	0x07,0x55,0x10,0x0C,0xCE,0x74
	DT	0x08,0x2A,0x0B,0x9B,0xBE,0x2D
	DT	0x09,0x50,0x14,0xFC,0xB9,0xA0
	DT	0x0A,0x68,0x12,0x60,0xB3,0x8B
	DT	0x0D,0x82,0x0E,0x4E,0xA8,0xF5
	DT	0x11,0xC1,0x0A,0x2E,0x9B,0x05
	DT	0x16,0xDF,0x11,0x7D,0x94,0xA7
	DT	0x20,0x8A,0x0C,0x79,0x86,0x4F
	DT	0x2F,0x2B,0x0F,0x15,0x72,0x3F
	DT	0x39,0xB2,0x0B,0x16,0x66,0x7A
	DT	0x52,0x36,0x08,0x6E,0x5C,0xE5
	DT	0x6E,0xBE,0x0B,0x6E,0x4E,0xF1
	DT	0x8C,0xE0,0x07,0xEC,0x42,0xCD
	DT	0xFF,0xFF,0x0B,0x12,0x38,0x48

; Big Endian
P1Spline
;	DT	d'30'	; count
	DT	0x00,0x46,0xFF,0xFF,0x4D,0xDA
	DT	0x00,0x54,0xE1,0xEF,0x45,0xA1
	DT	0x00,0x5D,0xC4,0x3D,0x3B,0xE3
	DT	0x00,0x69,0xA2,0x13,0x2F,0x7A
	DT	0x00,0x7D,0x8B,0x85,0x26,0x39
	DT	0x00,0x95,0x72,0xB9,0x1A,0x1E
	DT	0x00,0xA1,0x5C,0x6B,0x0D,0x23
	DT	0x00,0xA9,0x51,0xFA,0x06,0x91
	DT	0x00,0xB8,0x58,0x0C,0x0A,0x93
	DT	0x00,0xCA,0x4C,0x7E,0x02,0x45
	DT	0x00,0xE8,0x41,0x3F,0xF9,0x65
	DT	0x01,0x1D,0x38,0x08,0xF1,0x0C
	DT	0x01,0x45,0x29,0x33,0xE0,0x89
	DT	0x01,0x4A,0x23,0x49,0xD9,0x06
	DT	0x01,0x51,0x30,0x7A,0xEA,0x08
	DT	0x01,0x5E,0x2D,0xF3,0xE6,0xB4
	DT	0x02,0x06,0x1E,0xCA,0xD1,0xF9
	DT	0x02,0x3D,0x16,0x2E,0xC0,0x8E
	DT	0x02,0xD0,0x13,0x92,0xBA,0xB8
	DT	0x03,0x96,0x0E,0x35,0xAB,0xA2
	DT	0x04,0x9F,0x0A,0x73,0x9E,0x26
	DT	0x05,0xE0,0x11,0xD2,0x97,0x09
	DT	0x08,0x31,0x0C,0xFF,0x88,0xDD
	DT	0x0B,0x99,0x10,0x58,0x75,0x1C
	DT	0x0E,0x0A,0x0B,0x21,0x65,0xFC
	DT	0x10,0xAA,0x0A,0xF7,0x65,0x69
	DT	0x13,0x98,0x10,0x54,0x59,0xBE
	DT	0x19,0xD7,0x0D,0x35,0x52,0x18
	DT	0x20,0x66,0x09,0x45,0x45,0x62
	DT	0xFF,0xFF,0x09,0x6E,0x32,0xEE

str_Wazzup	DT "   Key Down",0
str_Keyup	DT "    Key Up ",0

btnThread
	ScheduleTime	2, ButtonTime
	BCF 	INTCON, RBIE	; disable interrupts while we get next char
	movf	nButts, w
	btfss	STATUS, Z	; was it zero?
	GOTO	btnGotOne	; no, process it
	BSF 	INTCON, RBIE	; turn interrupt back on
	GOTO	btnDone 	; nothing to do
btnGotOne
	addlw	Butts-1
	movwf	FSR
	BANKISEL	Butts	; clr IRP
	movf	INDF, w
	movwf	AL
	decf	nButts, f
	BSF 	INTCON, RBIE	; reenable

	btfsc	AL,6
	GOTO	nobutt
	bcf 	PORTA, 3	; turn on buzzer
	MOVLW	d'20'
	CALL	LCDGoto
	movlw	high(str_Wazzup)
	movwf	PCLATH
	movlw	low(str_Wazzup)
	call	LCDPutCStr
	GOTO	btnDone

nobutt

	MOVLW	d'20'
	CALL	LCDGoto
	movlw	high(str_Keyup)
	movwf	PCLATH
	movlw	low(str_Keyup)
	call	LCDPutCStr

	bsf 	PORTA, 3	; turn on buzzer

btnDone
	goto	mainloop
END
