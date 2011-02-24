;----------------------------------------------------------------------
; math.asm
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
; The Original Code is some basic math operations.
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

	#include <p16f88.inc>
	radix dec
	#include "isr.inc"
	#define MATH_ASM
	#include "math.inc"

_DATA UDATA ;0x0C
PROD	res	4
BCD	res	3

math CODE

HexDigit
	movwf	AX
	movlw	9
	movwf	AX+1
	movf	AX,w
	andlw	0x0f
	subwf	AX+1,f
	BTFSS	STATUS,0	; skip if positive
	addlw	'A' - '0' - 10
	addlw	'0'
	return


; DMC: Mult16x24 was taken from somewhere on the internet.
; DMC: It's a very basic routine and I modified it heavily.
; DMC: I don't think someone could ever stake a claim to it
Mult16x16
	CLRF	BX+2
Mult16x24
	CLRF	PRODX
	CLRF	PRODU
	CLRF	PRODH
	CLRF	PRODL
	MOVLW	d'16'
	MOVWF	CL
Multiply_loop
	BCF	STATUS,C               
	RLF	PRODL, f
	RLF	PRODH, f
	RLF	PRODU, f
	RLF	PRODX, f
	BCF	STATUS,C
	RLF	AL, f
	RLF	AH, f
	BTFSS	STATUS,C
	GOTO	Multiply_skip
	MOVF	BX,w
	ADDWF	PRODL, f
	MOVF	BX+1, w
	BTFSC	STATUS,C
	INCFSZ	BX+1, w
	ADDWF	PRODH, f
	MOVF	BX+2, w
	BTFSC	STATUS,C
	INCFSZ	BX+2, w
	ADDWF	PRODU, f
	BTFSC	STATUS,C
	INCF	PRODX, f
Multiply_skip
	DECFSZ	CL, f
	GOTO	Multiply_loop
	RETLW	0h
             


; DMC: Divide was taken from somewhere on the internet.
; DMC: It's a very basic routine and I don't know if it even works
; DMC: I doubt someone could ever stake a claim to it

;******************************************************************
;; DIVIDE
; Divide one 16-bit number into a 16-bit number, returning the 16-bit result and 
; the remainder. Upon entry, the top of the division fraction (the dividend)
; must be in topH and topL, and the bottom (divisor) in btmH and btmL. 
; If division by zero is attempted, the routine will return immediately with 
; the error code of 0FFh in w. Otherwise, it will perform the division, leaving 
; the remainder in topH and topL and the result (quotient) in qH and qL. 
; Upon return from a successful division, w contains 0. 

;Div16
;	MOVF 	btmH,w                ; Check for division by 0.
;	IORWF	btmL,w               
; 	BTFSS	STATUS,Z
; 	GOTO	DIV_ok
; 	MOVLW	d'255'
; 	MOVWF	qH
; 	MOVWF	qL
; 	RETURN              ; Error code= 255: return. 

; DIV_ok:
; 	MOVLW d'1'                 ; Otherwise, initialize variables
;  	MOVWF CL
; 	CLRF qH                    
; 	CLRF qL                    

; Divide_sh_loop
; 	BTFSC btmH, 7         ; Shift divisor left
; 	GOTO Divide_d1
; 	BCF STATUS,C               ; until msb is in 
; 	RLF btmL,f                   ; btmH.7. 
; 	RLF btmH,f                   ; count = no. of shifts+1. 
; 	INCF CL,f                 
; 	GOTO Divide_sh_loop

; Divide_d1
; 	BCF STATUS,C               
; 	RLF qL,f                     ; Shift quotient left.
; 	RLF qH,f
;     ; top = top - btm. 
; 	SUBWORDS	topL, btmL
; 	BTFSS STATUS,C
; 	goto Divide_less
; 	BSF qL,0

; Divide_reentr
; 	BCF STATUS,C               
; 	RRF btmH,f
; 	RRF btmL,f
; 	DECFSZ CL,f
; 	GOTO Divide_d1
; 	RETLW 0h                   ; Return w/ remainder in top
                                        ; and result in q.&nsp;
; Divide_less
; 	ADDWORDS	topL, btmL
; 	goto    Divide_reentr
 
; Rem2Frac
; 	MOVLW	d'8'
;  	MOVWF	CL
; r2f_1:
;  	BTFSC	topH, 7
; 	GOTO	r2f_2
; 	BCF 	STATUS,C               
; 	RLF 	topL,f
; 	RLF 	topH,f
; 	GOTO	r2f_3
; r2f_2:
; 	BCF 	STATUS,C               
; 	RRF 	btmH,f
; 	RRF 	btmL,f
; r2f_3:
; 	DECFSZ	CL,f
; 	GOTO	r2f_1
; 	CALL	Div16
; 	RETURN

Div32_16
	CLRF	btmU
	CLRF	btmX
Div32
	MOVF 	btmL,w                ; Check for division by 0.
	IORWF	btmH,w               
	IORWF	btmU,w
	IORWF	btmX,w               
	BTFSS	STATUS,Z
	GOTO	DIV32_ok
	MOVLW	d'255'
	MOVWF	qL
	MOVWF	qH
	MOVWF	qU
	MOVWF	qX
	RETURN              ; Error code= 255: return. 

DIV32_ok:
	MOVLW d'1'                 ; Otherwise, initialize variables
 	MOVWF CL
	CLRF qL                    
	CLRF qH                    
	CLRF qU
	CLRF qX                    

Div32_sh_loop
	BTFSC btmX, 7         ; Shift divisor left
	GOTO Div32_d1
	BCF STATUS,C               ; until msb is in 
	RLF btmL,f
	RLF btmH,f
	RLF btmU,f
	RLF btmX,f
	INCF CL,f                 
	GOTO Div32_sh_loop

Div32_loop
	BCF STATUS,C               
	RRF btmX,f
	RRF btmU,f
	RRF btmH,f
	RRF btmL,f

Div32_d1
	BCF STATUS,C               
	RLF qL,f                     ; Shift quotient left.
	RLF qH,f
	RLF qU,f
	RLF qX,f
    ; top = top - btm. 
	SUB32	topL, btmL
	BTFSS STATUS,C
	goto Div32_less
	BSF qL,0

Div32_reentr
	DECFSZ CL,f
	GOTO Div32_loop
	RETLW 0

Div32_less
	ADD32	topL, btmL
	goto    Div32_reentr


; DMC: Bin2BCD was taken from somewhere on the internet.
; DMC: It's a very basic routine and I don't think
; DMC: someone could ever stake a claim to it
;
; Binary to BCD conversion routine
; 16 bit number to convert is in NumbHi, NumbLo
; result is set in BCD HML
;
#define MCount	CL
#define NumbHi	AH
#define NumbLo	AL

Bin2BCD
  	movlw	d'16'
  	movwf	MCount
  	clrf	BCD
  	clrf	BCD+1
  	clrf	BCD+2
	goto	B2D_loop

B2D_adjDEC
   movlw BCD+2
   movwf FSR
   call B2D_adjBCD
   call B2D_adjBCD
   call B2D_adjBCD

B2D_loop
   bcf STATUS,C
   rlf NumbLo,F
   rlf NumbHi,F
   rlf BCD,F
   rlf BCD+1,F
   rlf BCD+2,F
   decfsz MCount,F
   goto	B2D_adjDEC
   return

B2D_adjBCD
	movlw	0x33	; Add 3 to each nibble
	addwf	INDF,F	;
	btfsc	INDF,3	; test if low result > 7
	andlw	0xf0	; low result >7 so don't take the 3 out
	btfsc	INDF,7	; test if high result > 7
	andlw	0x0f	; high result >7 so don't take the 3 out
	subwf	INDF,F	; any results <= 7, subtract back
	decf	FSR,f	; advance to the next
	return


; InterpolateSpline is (c) David M. Clay
InterpolateSpline
	movwf	FSR
	movlw	1
	movwf	DU	; prime uppermost intercept bit
	CLRF	CH	; save hibyte of previous knot point in CH!
ISloop:	
	call	nxtchr	; knotH
	movwf	AX+1	; big endian now!
	movwf	DH		; save hibyte of matching knot point in DH!
	call	nxtchr	; knotL
	movwf	AX

	SUBWORDS	AX,BX
	BTFSC	STATUS,C
	goto	ISok	; ax - rCount >= 0 (positive), rCount <= AX
	; result was negative rCount > AX, so keep going
	movf	DH,w
	movwf	CH	; save hibyte of previous knot point in CH!
	call	nxtchr	; slopeH
	call	nxtchr	; slopeL
	call	nxtchr	; interceptH
	andlw	0x80	; did we flip? so we can comput upper bit?
	BTFSS	STATUS,Z
	clrf	DU  	; we flipped, so clear the upper bit
	call	nxtchr	; interceptL
	goto	ISloop

ISok:
ISdone:
	call	nxtchr	; slopeH
	movwf	AH
	call	nxtchr	; slopeL
	movwf	AL
	call	nxtchr	; interceptH
	movwf	DH
	andlw	0x80	; did we flip? so we can comput upper bit?
	BTFSS	STATUS,Z
	clrf	DU  	; we flipped, so clear the upper bit
	call	nxtchr	; interceptL
	movwf	DL

	; Shift BX to 10 significant bits, based on CH from the matching knot point
	goto	ISshtest
ISshift
	BCF 	STATUS,C
	RRF 	CH,f
	BCF 	STATUS,C
	RRF 	BH,f
	RRF 	BL,f
ISshtest:
	movf	CH,w
	andlw	0xfc
	BTFSS	STATUS,Z
	goto	ISshift

	; T = intercept + BX * slope (and for us slope will be negative!)
	; T = DX - BX * AX
	CALL	Mult16x16	; PROD = AX * BX
	SUB24	DX,PROD+1	; DX = DX - PROD/256
	return

END

