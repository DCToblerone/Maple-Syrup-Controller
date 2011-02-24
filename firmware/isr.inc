;----------------------------------------------------------------------
; isr.inc
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

ifndef ISR_ASM
	extern	nxtchr
;	extern	ISRFlags
	extern	TMR0H
	extern	TMR0U
	extern	AX
	extern	BX
	extern	CX
	extern	DX
	extern	Butts
	extern	nButts

	extern	internalDelayUs
	extern	DelayMs
	extern	EERead
	extern	EEWrite
	extern	DispMode
endif

; bits set in DispMode.. some are mutually exclusive, some are not
#define DM_MAIN	0
#define DM_TOTALS 1
#define DM_SETUP1 2
#define DM_SETUP2 3
#define	DM_DEBUG 7

#define AL AX
#define AH (AX + 1)
#define BL BX
#define BH (BX + 1)
#define CL CX
#define CH (CX + 1)
#define DL DX
#define DH (DX + 1)
#define DU (DX + 2)

ISR_TMR0 EQU	1	; TMR0 flipped
ISR_TMR0H EQU	2	; TMR0H flipped

GetTS MACRO d
	BCF 	INTCON,TMR0IE	; disable the TMR0 interrupt so that TMR0 and TMR0H are good
	movf	TMR0, w
	movwf	d
	movf	TMR0H, w
	movwf	(d + 1)
	BSF 	INTCON,TMR0IE	; re-enable the TMR0 interrupt
 ENDM

GetTS1 MACRO d
	BCF 	INTCON,TMR0IE	; disable the TMR0 interrupt so that TMR0 and TMR0H are good
	movf	TMR0, w
	BSF 	STATUS, RP0	; BANK1
	movwf	d
	movf	TMR0H, w
	movwf	(d + 1)
	BSF 	INTCON,TMR0IE	; re-enable the TMR0 interrupt
 ENDM

; d -= TMR0H:TMR0
SubTS MACRO d
	BCF 	INTCON,TMR0IE	; disable the TMR0 interrupt so that TMR0 and TMR0H are good
	movf	TMR0, w 
	subwf	d,f 
	movf	TMR0H, w 
	btfss	STATUS, 0 
	incfsz	TMR0H,w 
	subwf	(d + 1), f
	BSF 	INTCON,TMR0IE	; re-enable the TMR0 interrupt
 ENDM

; 4-cycles
MAKEWORD MACRO d,msb,lsb
	movf	lsb, w 
	movwf	d 
	movf	msb, w 
	movwf	(d + 1)
 ENDM

MAKEWORDINV MACRO d,msb,lsb
	movf	lsb, w 
	xorlw	0xff 
	movwf	d 
	movf	msb, w 
	xorlw	0xff 
	movwf	(d + 1)
 ENDM

; d = -d
NEGWORD MACRO d
	comf	d, f
	comf	(d + 1), f
	incf	d,f 
	btfsc	STATUS, 2 
	incf	(d + 1), f 
 ENDM

; d -= WORD(msb,lsb)
SUBWORD MACRO d,msb,lsb
	movf	lsb, w 
	subwf	d,f 
	movf	msb, w 
	btfss	STATUS, 0 
	incfsz	msb ,w 
	subwf	(d + 1), f
 ENDM

SUB2416 MACRO d,s
	movf	s, w 
	subwf	d,f 
	movf	(s + 1), w 
	btfss	STATUS, 0 
	incfsz	(s + 1) ,w 
	subwf	(d + 1), f
	movlw	0
	btfss	STATUS, 0 
	movlw	1
	subwf	(d + 2), f
 ENDM

SUB24 MACRO d,s
	movf	s, w 
	subwf	d,f 
	movf	(s + 1), w 
	btfss	STATUS, 0 
	incfsz	(s + 1) ,w 
	subwf	(d + 1), f
	movf	(s + 2), w 
	btfss	STATUS, 0 
	incfsz	(s + 2) ,w 
	subwf	(d + 2), f
 ENDM

SUB32 MACRO d,s
	movf	s, w 
	subwf	d,f 
	movf	(s + 1), w 
	btfss	STATUS, 0 
	incfsz	(s + 1) ,w 
	subwf	(d + 1), f
	movf	(s + 2), w 
	btfss	STATUS, 0 
	incfsz	(s + 2) ,w 
	subwf	(d + 2), f
	movf	(s + 3), w 
	btfss	STATUS, 0 
	incfsz	(s + 3) ,w 
	subwf	(d + 3), f
 ENDM

; d += WORD(msb,lsb)
ADDWORD MACRO d,msb,lsb
	movf	lsb, w 
	addwf	d,f 
	movf	msb, w 
	btfsc	STATUS, C
	incfsz	msb ,w 
	addwf	(d + 1), f
 ENDM

ADDWORDS MACRO d,s
	movf	s, w 
	addwf	d,f 
	movf	(s + 1), w 
	btfsc	STATUS, C
	incfsz	(s + 1) ,w 
	addwf	(d + 1), f
 ENDM

ADD2416 MACRO d,s
	movf	s, w 
	addwf	d,f 
	movf	(s + 1), w 
	btfsc	STATUS, C 
	incfsz	(s + 1), w 
	addwf	(d + 1), f
	btfsc	STATUS, C 
	incf	(d + 2), f
 ENDM

ADD32 MACRO d, s
	movf	s, w 
	addwf	d,f 
	movf	(s + 1), w 
	btfsc	STATUS, C 
	incfsz	(s + 1), w 
	addwf	(d + 1), f
	movf	(s + 2), w 
	btfsc	STATUS, C 
	incfsz	(s + 2), w 
	addwf	(d + 2), f
	movf	(s + 3), w 
	btfsc	STATUS, C 
	incfsz	(s + 3), w 
	addwf	(d + 3), f
 ENDM

; d += lit;
ADDWORDLIT MACRO d,lit
	movlw	((lit)&0xff) 
	addwf	d,f 
	movlw	((lit)>>8) 
	btfsc	STATUS, 0 
	movlw	(((lit)>>8) + 1) 
	addwf	(d + 1), f
 ENDM

; d - lit
; C = 0 result is negative
; C = 1 result is positive (or zero)
; Z = 1 result is zero
CMPWORDLIT MACRO d,lit
	local done
	movlw	((lit)>>8) 
	subwf	d+1,w	; w = d[1] - lit[1]
	btfsc	STATUS, C
	btfss	STATUS, Z
	goto	done	; C = 0 result is negative or C = 1 and Z = 0 result is positive
	movlw	((lit)&0xff) 
	subwf	d,w	; w = d - lit
done:
 ENDM

CMPWORD MACRO d,s
	local done
	movf	s+1,w
	subwf	d+1,w	; w = d[1] - s[1]
	btfsc	STATUS, C
	btfss	STATUS, Z
	goto	done	; C = 0 result is negative or C = 1 and Z = 0 result is positive
	movf	s,w
	subwf	d,w	; w = d - lit
done:
 ENDM

CMP32 MACRO d,s
	local done
	movf	s+3,w
	subwf	d+3,w	; w = d[3] - s[3]
	btfsc	STATUS, C
	btfss	STATUS, Z
	goto	done	; C = 0 result is negative or C = 1 and Z = 0 result is positive
	movf	s+2,w
	subwf	d+2,w	; w = d[1] - s[1]
	btfsc	STATUS, C
	btfss	STATUS, Z
	goto	done	; C = 0 result is negative or C = 1 and Z = 0 result is positive
	movf	s+1,w
	subwf	d+1,w	; w = d[1] - s[1]
	btfsc	STATUS, C
	btfss	STATUS, Z
	goto	done	; C = 0 result is negative or C = 1 and Z = 0 result is positive
	movf	s,w
	subwf	d,w	; w = d - lit
done:
 ENDM

; d -= lit
SUBWORDLIT MACRO d,lit
	movlw	((lit)&0xff) 
	subwf	d,f 
	movlw	((lit)>>8) 
	btfss	STATUS, 0 
	movlw	(((lit)>>8) + 1)
	subwf	(d + 1), f
 ENDM

; d = lit - d
SUBLITWORD MACRO d,lit
	movf	d,w
	sublw	((lit)&0xff) 
	movwf	d
	movf	(d + 1), w
	btfss	STATUS, 0 
	incf	(d + 1), w
	sublw	((lit)>>8)
	movwf	(d + 1)
 ENDM

SUBLIT24 MACRO d,lit
	movf	d,w
	sublw	((lit)&0xff) 
	movwf	d
	movf	(d + 1), w
	btfss	STATUS, 0 
	incf	(d + 1), w
	sublw	(((lit)>>8) & 0xff)
	movwf	(d + 1)
	movf	(d + 2), w
	btfss	STATUS, 0 
	incf	(d + 2), w
	sublw	((lit)>>16)
	movwf	(d + 2)
 ENDM

INCWORD MACRO d
	incf	d,f
	btfsc	STATUS, Z
	incf	(d + 1), f 
 ENDM

MOVWORDFUNC MACRO d,func
	movlw	low (func) 
	movwf	d 
	movlw	high (func) 
	movwf	(d + 1)
 ENDM

MOVWORD MACRO d,s
	movf	s, w
	movwf	d 
	movf	(s + 1),w
	movwf	(d + 1)
 ENDM

MOV24 MACRO d,s
	movf	s, w
	movwf	d 
	movf	(s + 1),w
	movwf	(d + 1)
	movf	(s + 2),w
	movwf	(d + 2)
 ENDM

MOV24R MACRO d,s
	movf	(s + 2),w
	movwf	(d + 2)
	movf	(s + 1),w
	movwf	(d + 1)
	movf	s, w
	movwf	d 
 ENDM

SUBWORDS MACRO d,s
	movf	s, w 
	subwf	d,f 
	movf	(s + 1), w 
	btfss	STATUS, 0 
	incfsz	(s + 1) ,w 
	subwf	(d + 1), f
 ENDM

MAKEWORDINDF MACRO d
	movf	INDF, w 
	movwf	d 
	incf	FSR,f 
	movf	INDF, w 
	movwf	(d + 1)
	incf	FSR,f
 ENDM

STOREBYTEINDF MACRO s
	movf	s, w 
	movwf	INDF 
	incf	FSR,f
 ENDM

STOREWORDINDF MACRO s
	movf	s, w 
	movwf	INDF 
	incf	FSR,f 
	movf	(s + 1), w 
	movwf	INDF
	incf	FSR,f
 ENDM

; d = lit;
SETWORDLIT MACRO d,lit
	movlw	((lit)&0xff) 
	movwf	d
	movlw	((lit)>>8) 
	movwf	(d + 1)
 ENDM

BANK0 MACRO
	bcf	STATUS, RP0
	bcf	STATUS, RP1
 ENDM

BANK1 MACRO
	bsf	STATUS, RP0
	bcf	STATUS, RP1
 ENDM

BANK2 MACRO
	bcf	STATUS, RP0
	bsf	STATUS, RP1
 ENDM

BANK3 MACRO
	bsf	STATUS, RP0
	bsf	STATUS, RP1
 ENDM

#define TMR0Prescaler 7
; TMR0Prescaler      ndef 0 1 2  3  4  5   6   7
; divide TMR0 by        1 2 4 8 16 32 64 128 256
ifdef TMR0Prescaler
    #define CyclesPerTMR0 (2<<TMR0Prescaler)
else
    #define CyclesPerTMR0 1
endif

; 3.36864MHz clock
; 4 clock pulses per instruciton cycle
; For very accurate timing, you should make CMult as small as you can w/o sacrficing
; significant digits, while not losing any sig digs in CDiv
; (i.e. divide by largest common power of 2)
; make sure HzSig >= HdSig too
#define Hz 3686400	; 2^14 * 3^2 * 5^2
#define HzSig 14	; 14 twos

#define Hd 4000000	; 2^8 * 5^6
#define HdSig 8		; 8 twos

#define CMult (Hz/(1<<HdSig)/5/5)	; 576 (2^6 * 3^2)
#define CMultSig (HzSig - HdSig)	; 6 twos
#define CDiv (Hd/(1<<HdSig)/5/5)	; 625 (5^4)
#define CDivSig 0	; 0 twos

; Divide out any CyclesPerTMR0 that we can first, so we can multiply
; big numbers like 1,000,000 and not have it overflow the 32-bit limit
; on the preprocessor's expression evaluator!

if (CMultSig > TMR0Prescaler+1)
	#define TMR0Sig	(TMR0Prescaler+1)
else
	#define TMR0Sig	CMultSig	; 6
endif
#define TMR0Mult (CMult/(1<<TMR0Sig))
#define TMR0Div (CDiv*(1<<(TMR0Prescaler+1-TMR0Sig)))
#define TMR0DivSig (CDivSig + (TMR0Prescaler+1-TMR0Sig))

if (TMR0DivSig > 8)
	#define RemSig	8
else
    if (TMR0DivSig > 0)
		#define RemSig TMR0DivSig
	else
		#define RemSig 0
	endif
endif

#define CyclesPerMicro CMult/CDiv
#define MicrosPerCycle CDiv/CMult

#define CyclesPerTMR0H CyclesPerTMR0*256
#define MicrosPerTMR0 CyclesPerTMR0*MicrosPerCycle

#define TMR0PerMicro TMR0Mult/TMR0Div
#define TMR0PerMicroRemainder TMR0Mult*(1<<(8-RemSig))/(TMR0Div/(1<<RemSig))

;#define TMR0PerMicro CyclesPerMicro/CyclesPerTMR0
;#define TMR0PerMicroRemainder CyclesPerMicro*256/CyclesPerTMR0

; see internalDelayUs in isr.asm
if (MicrosPerCycle < 2)
 	#define InternalDelayCycles 4
else
	#define InternalDelayCycles 3
endif

#define DelayUsCycles ((x) * CyclesPerMicro - 6)
DelayUs MACRO x
	movlw	(DelayUsCycles - 3) / InternalDelayCycles
	call	internalDelayUs
 ENDM
