;----------------------------------------------------------------------
; math.inc
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

	extern Mult16x16
	extern HexDigit
	extern PROD
	extern Divide
	extern Bin2BCD
	extern BCD
	extern InterpolateSpline

#define PRODL PROD
#define PRODH (PROD + 1)
#define PRODU (PROD + 2)
#define PRODX (PROD + 3)

#define BCDL BCD
#define BCDH (BCD + 1)
#define BCDU (BCD + 2)

