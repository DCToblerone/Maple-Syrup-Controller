;----------------------------------------------------------------------
; sched.inc
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

	extern mainloop
	extern proc0;
	extern proc1;
	extern proc2;

;typedef struct {
;	byte flag;	// 0 means disabled
;	int timeout;
;	word ttr;
;	void (*func)(void);
;   byte frac; // only proc0 has this for accurate timing with fractional tmr0 counts
;} PROC;

ScheduleTime MACRO p, _tmo
;	BSF 	STATUS, RP0	; BANK1
	GetTS1 (proc#v(p) + 1)
	ADDWORDLIT proc#v(p) + 1, (_tmo) * TMR0PerMicro
	bsf proc#v(p), 0 
	BCF 	STATUS, RP0	; BANK0
 ENDM

ScheduleAddTime MACRO p, _tmo
	BSF 	STATUS, RP0	; BANK1
	bsf proc#v(p), 0
	ADDWORDLIT proc#v(p) + 1, (_tmo) * TMR0PerMicro
	BCF 	STATUS, RP0	; BANK0
 ENDM

ScheduleAddAccurateTime MACRO p, _tmo
	LOCAL next
	BSF 	STATUS, RP0	; BANK1
	bsf proc#v(p), 0
	ADDWORDLIT proc#v(p) + 1, (_tmo) * TMR0PerMicro
;	movlw	(((_tmo << 8) * TMR0PerMicro) & 0xff) 
	movlw	(((_tmo) * TMR0PerMicroRemainder) & 0xff)
	addwf	proc#v(p) + 7,f	; frac
	btfss	STATUS, 0 
	GOTO	next
	INCWORD proc#v(p) + 1
next:
	BCF 	STATUS, RP0	; BANK0
 ENDM

Schedule MACRO p, _flag, _tmo, _ttr, _func
;	BSF 	STATUS, RP0	; BANK1
	GetTS1 (proc#v(p) + 1)
	ADDWORDLIT proc#v(p) + 1, (_tmo) * TMR0PerMicro
	movlw	_flag
	movwf	proc#v(p)
	bsf	proc#v(p), 0
	SETWORDLIT proc#v(p) + 3, (_ttr) * TMR0PerMicro
	MOVWORDFUNC proc#v(p) + 5, _func
	BCF 	STATUS, RP0	; BANK0
 ENDM

ScheduleWait MACRO p, _tmo, _ttr
	LOCAL next
;	BSF 	STATUS, RP0	; BANK1
	GetTS1 (proc#v(p) + 1)
	ADDWORDLIT proc#v(p) + 1, (_tmo) * TMR0PerMicro
	bsf	proc#v(p), 0
	SETWORDLIT proc#v(p) + 3, (_ttr) * TMR0PerMicro
;	movlw	low($+5)
	movlw	low(next)
	movwf	proc#v(p) + 5
;	movlw	high($+3)
	movlw	high(next)
	movwf	proc#v(p) + 6
	BCF 	STATUS, RP0	; BANK0
	goto	mainloop
next:
 ENDM

ScheduleWaitForTimeout MACRO p
	LOCAL next, out
	BSF 	STATUS, RP0	; BANK1
	btfss	proc#v(p), 0
	goto	out
	movlw	low(next)
	movwf	proc#v(p) + 5
	movlw	high(next)
	movwf	proc#v(p) + 6
	BCF 	STATUS, RP0	; BANK0
	goto	mainloop
out:
	BCF 	STATUS, RP0	; BANK0
next:
 ENDM
