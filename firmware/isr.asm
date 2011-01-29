;----------------------------------------------------------------------
; isr.asm
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
; The Original Code is the interrupt service routine for the
; maple syrup controller.
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
	#define ISR_ASM
	#include "isr.inc"	; MicrosPerCycle, et al.
	radix dec

;--------------------------------------------------------
; global declarations
;--------------------------------------------------------
	global	isr
;	global	ISRFlags
	global	TMR0U
	global	TMR0H
	global	AX
	global	BX
	global	CX
	global	DX
	global	Butts
	global	nButts
	global	nxtchr
	global	DelayMs
	global	internalDelayUs
	global	EERead
	global	EEWrite

;--------------------------------------------------------
; global definitions
;--------------------------------------------------------
; Shared data available in all banks
_SHARED udata_ovr 0x70
; DMC: really only WSAVE is needed to be shared since SaveContext
;      immediately switchs to Bank0 after WREG is saved to WSAVE
WSAVE	res	1	; WREG

_DATA UDATA ;0x0C
PSAVE	res	1	; PCLATH
SSAVE	res	1	; STATUS

AX	res	2
BX	res	2
CX	res	2
DX	res	3

TMR0H	res	1
TMR0U	res	1
;RB0Time	res	2
btntmp	res	1
nButts	res	1
Butts	res	4	; make this a power of 2 so that incf nButts will wrap!
;ISRFlags	res	1

SaveContext	MACRO
	movwf	WSAVE ; save off current 0x0000 register contents
	swapf	STATUS,w ; move status register into 0x0000 register
	clrf	STATUS	; bank 0 (clears IRP RP1 RP0) (DMC: not really needed as long as isr can operate in any bank)
	movwf	SSAVE
	movf	PCLATH,w
	movwf	PSAVE
	clrf	PCLATH	; page 0, absolutely essential if we have any GOTOs
 ENDM

RestoreContext	MACRO
	movf	PSAVE,w
	movwf	PCLATH
	swapf	SSAVE,w
	movwf	STATUS
	swapf	WSAVE,f ; DMC: swapf is used since it doesn't change the STATUS register
	swapf	WSAVE,w ; restore pre-isr w contents
 ENDM

;--------------------------------------------------------
; code
;--------------------------------------------------------
; interrupt vector
INT_VECTOR CODE 0x004
isr	; Interrupt Service Routine
	; DMC: No matter what, I MUST save context here if I am ever to use GOTO
	; in the interrupt when running code in PAGE 1! If I had only TMR0 handler
	; w/o any GOTOs then I could get away with just a RETFIE
	SaveContext
	btfss INTCON, TMR0IF ; does not change STATUS
	goto ISR_10	; PAGESEL here???!?!?! With SaveContext, page is set to 0
	bcf INTCON, TMR0IF	; T0IF
;	bsf	ISRFlags, 0	; let timing routines know that the interrupt happened!
	incfsz TMR0H,f ; does not change STATUS
	goto	ISR_EXIT
;	retfie ; return from interrupt
;	bsf	ISRFlags, 1	; let timing routines know that TMR0H flipped!
	incfsz TMR0U,f ; does not change STATUS
	goto	ISR_EXIT
;	retfie ; return from interrupt
	goto	ISR_EXIT
;	retfie ; return from interrupt (incase we skipped above)

ISR_10:
;	btfss INTCON, INT0IF ; does not change STATUS
;	goto	ISR_20:
;	; ---- INT0 Interrupt from RB0
;	goto	ISR_EXIT

ISR_20:
	btfss INTCON, RBIF ; does not change STATUS
	goto	ISR_EXIT
	movf	PORTB, w	; read this as early as possible so we don't lose it!
	andlw	0x70	; clear unrelated bits
	movwf	btntmp	; save for later if we add it to the queue
	xorwf	Butts+0, w	; is it the same as what we have first in the queue?
	BTFSC	STATUS, Z	; Z==1 means they were the same
	GOTO	ButtsFull	; same, so get out!
	movf	nButts, w
	sublw	4	; is the queue full?
	BTFSC	STATUS, Z	; z==1 means nButts==4
	GOTO	ButtsFull	; full, so get out!
	; insert into front of buffer
	movf	Butts+2, w
	movwf	Butts+3
	movf	Butts+1, w
	movwf	Butts+2
	movf	Butts+0, w
	movwf	Butts+1
	movf	btntmp, w
	movwf	Butts+0
	incf	nButts, f
ButtsFull
	BCF 	INTCON, RBIF
;	goto	ISR_EXIT
ISR_EXIT
	RestoreContext
	retfie ; return from interrupt

PROG CODE


nxtchr
	movf FSR,w
	incfsz FSR,f	; increment FSR and skip if PCLATH needs to be incremented
	movwf	PCL 	; Indirect jump
	call 	$-1		; Here we skipped, so we neeed an indirect CALL so we can
	INCF	PCLATH,f; increment PCLATH after we get back from the call!
    return

DelayMs
	; if (mS) do {
	IORLW	0
	BTFSC	STATUS,Z
	GOTO	DMS_else
	; do {
	movwf	AX
DMS_do:
		DelayUs 1000
		; } while (--mS) ;
		DECFSZ	AX,F
		GOTO	DMS_do
DMS_else:
	RETURN	

internalDelayUs
	; if (mS) do {
	IORLW	0
	BTFSC	STATUS,Z
	GOTO	IDU_else
	; do {
	movwf	AX+1
IDU_do:
if (MicrosPerCycle<2)
		NOP
endif
		; } while (--uS) ;
		DECFSZ	AX+1,F
		GOTO	IDU_do
IDU_else:
	RETURN	

; Read EEPROM memory
; input: w = EEPROM offset 00 - 3f, FSR set to destination
; output: data read into INDF and w++ and FSR++
EERead
	BANKSEL	EEADR
	movwf	EEADR
	BANKSEL	EECON1
	bcf 	EECON1, EEPGD
	bsf 	EECON1, RD
	BANKSEL	EEDATA
	movf	EEDATA, w
	movwf	INDF
	incf	FSR, f
	btfsc	STATUS, Z
	bsf 	STATUS, IRP
	incf	EEADR, w
	BANKSEL	0
	RETURN

; Write EEPROM memory
; input: w = EEPROM offset 00 - 3f, FSR set to source
; output: data at INDF written and w++ and FSR++
EEWrite
	BANKSEL	EECON1
	btfsc	EECON1, WR	; wait for any previous write to complete
	goto	$-1
	BANKSEL	EEADR
	movwf	EEADR
	movf	INDF, w
	movwf	EEDATA
	BANKSEL	EECON1
;	bcf 	EECON1, EEIF	; make sure this is clear
	bcf 	EECON1, EEPGD
	bsf 	EECON1, WREN
	bcf 	INTCON, GIE
	MOVLW	0x55
	MOVWF	EECON2
	MOVLW	0xAA
	MOVWF	EECON2
	bsf 	EECON1, WR
	bsf 	INTCON, GIE
	bcf 	EECON1, WREN
	incf	FSR, f
	btfsc	STATUS, Z
	bsf 	STATUS, IRP
	BANKSEL	EEADR
	incf	EEADR, w
	BANKSEL	0
	RETURN

END
