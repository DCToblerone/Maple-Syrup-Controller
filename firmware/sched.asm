;----------------------------------------------------------------------
; sched.asm
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
; The Original Code handles basic thread scheduling
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
	radix dec
	include "isr.inc"

	global	mainloop
	global	proc0
	global	proc1
	global	proc2

SCHED_DATA	udata_ovr
proc0	res	8	; 8 for extra frac variable
proc1	res	7
proc2	res	7

_DATA	udata
mto	res	2

DoDispatch MACRO p
	MOVLW	proc#v(p)
	CALL	Dispatch
	IORLW	0 
	BTFSS	STATUS,Z
	GOTO	DPC_indjmp
 ENDM

code_sched	code

mainloop	;Function start

	; mto = MAX_TIMEOUT_VALUE;
	MOVLW	0xff
	MOVWF	mto
	MOVWF	(mto + 1)
	DoDispatch 0
	DoDispatch 1
	DoDispatch 2
	GOTO	mainloop

; PROC ptr in w
; byte Dispatch (byte p)
Dispatch	;Function start
	MOVWF	FSR
	; "sched.c"	if (p->flag)
	BTFSS	INDF,0
	GOTO	DPC_else1
		; advance to p->timeout
		INCF	FSR,F
		MAKEWORDINDF	AX
		SubTS AX
		; if (AX & 0x8000) {
		BTFSS	(AX + 1),7
		GOTO	DPC_else2
			MAKEWORDINDF	BX
			SUBWORDS	BX, mto
			; if (!C) i.e if (BX < mto)
			BTFSC	STATUS, C
			GOTO	DPC_else3
;				call DPC_indjmp
				retlw 1
DPC_indjmp:
				; FSR -= 5;// get to p->flag
				MOVLW	0xfb
				ADDWF	FSR,F
				; INDF = INDF & ~0x01; // clear the active flag
				BCF	INDF,0
				MOVLW	0x06	; FSR += 6 get to MSB of p->func
				ADDWF	FSR,F
				MOVF	INDF,W	; PCL = p->func
				MOVWF	PCLATH
				DECF	FSR,F	; get to LSB of p->func
				MOVF	INDF,W
				MOVWF	PCL	; jumps to p->func and returns above
DPC_else3:
			retlw 0 
DPC_else2:
			; if (AX < mto) mto = AX;// (i.e. timeleft < MinTimeout)
			MOVF	(mto + 1),W
			SUBWF	(AX + 1),W
			BTFSS	STATUS,Z
			GOTO	_00119_DS_
			MOVF	mto,W
			SUBWF	AX,W
_00119_DS_:
			BTFSC	STATUS,C
			GOTO	_00112_DS_
			MOVWORD	mto, AX
_00112_DS_:
DPC_else1:
	retlw 0 

end