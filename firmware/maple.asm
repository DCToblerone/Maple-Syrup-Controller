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
	#include "thermo.inc"
	global	eSecs


; -----------------------------------------------------------------------
; Configuration bits: adapt to your setup and needs
;	__CONFIG _HS_OSC & _WDT_OFF & _PWRTE_OFF & _CP_OFF
	__CONFIG	_CONFIG1, _CP_OFF & _CCP1_RB0 & _DEBUG_OFF & _WRT_PROTECT_OFF & _CPD_OFF & _LVP_OFF & _BODEN_OFF & _MCLR_OFF & _PWRTE_OFF & _WDT_OFF & _EXTCLK
	__CONFIG	_CONFIG2, _IESO_OFF & _FCMEN_OFF


;	global	clk

; -----------------------------------------------------------------------
; Variables declaration
DEEPROM CODE	; 0x2100
;tMins	dw	0x00	; Total minutes 
;tHours	dw	0x00	; Total Hours
;tHoursH	dw	0x00


_DATA UDATA
eSecs	res	2	; elapsed seconds today
clk	res	3

;tPos	RES	1

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

;str_Title	DT "  Maple Controller",0
str_Title	DT "  Shepherd's Corner",0

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

	CLRF	clk
	CLRF	clk+1
	CLRF	clk+2
	CLRF	eSecs
	CLRF	eSecs+1

	CALL	LCDInit
	CALL	ThermoInit

	clrf	DispMode	
	BSF 	DispMode, DM_MAIN
	CALL	SwitchDisplay

	; thread 0 will call the clock routine once every second
	Schedule	0,1,1000000,20000,clkThread

	; thread 1 will run nearly continuously
	Schedule	1,1,DischargeTime,1000,ThermoThread

	; thread 2 will call the button routines often!
	Schedule	2,1,ButtonTime,20000,btnThread

	; turn things over to the scheduler
	goto	mainloop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; clkThread
;
clkThread
	; come back here exactly 1 second from now!
	ScheduleAddAccurateTime	0, 1000000

	incf	eSecs,f
	BTFSC	STATUS,Z
	incf	eSecs+1,f
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
	BTFSC	DispMode, DM_MAIN
	CALL	clkOut
	GOTO	mainloop ; allow other threads to run

clkOut
	MOVLW	d'64'
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
	RETURN

SwitchDisplay
	BTFSS	DispMode, DM_MAIN
	GOTO	sd_1
	; Main display screen
	;/--------------------\
	;| Shepherd's Corner  |
	;|04:29:34       33.5°|
	;|132, 14.3gph  218.7°|
	;|238, 2.1gal  16.2gph|
	;\--------------------/
	CALL	LCDClear
	movlw high(str_Title)
	movwf PCLATH
	movlw low(str_Title)
	call LCDPutCStr
	call clkOut
	call pumpOut
	call thermoOut
	GOTO	sd_done
sd_1:
	BTFSS	DispMode, DM_TOTALS
	GOTO	sd_2
	GOTO	sd_done
sd_2:
	BTFSS	DispMode, DM_SETUP1
	GOTO	sd_3
	GOTO	sd_done
sd_3:
	BTFSS	DispMode, DM_SETUP2
;	GOTO	sd_4

;	GOTO	sd_done
sd_done:
	RETURN

;"Tank Fill      >8000"
;"Tank Stop       <100"
;"Pump Vol.     2.2gal"
;"Drawoff Temp   219.1"
;"Debug Mode        On"
;"Tank Fill           "
;"Tank Stop           "
;"Drawoff Temp        "



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
;	MOVLW	d'20'
;	CALL	LCDGoto
;	movlw	high(str_Wazzup)
;	movwf	PCLATH
;	movlw	low(str_Wazzup)
;	call	LCDPutCStr
	GOTO	btnDone

nobutt

;	MOVLW	d'20'
;	CALL	LCDGoto
;	movlw	high(str_Keyup)
;	movwf	PCLATH
;	movlw	low(str_Keyup)
;	call	LCDPutCStr

	bsf 	PORTA, 3	; turn off buzzer
	movlw	(1<<DM_DEBUG)
	xorwf 	DispMode, f
	btfss	DispMode, DM_DEBUG
	CALL	SwitchDisplay	; debug was turned off, so update display


btnDone
	goto	mainloop
END
