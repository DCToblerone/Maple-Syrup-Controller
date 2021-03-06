;----------------------------------------------------------------------
; lcd.inc
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
; The Original Code is the LCD interface module include.
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

	extern	LCDInit
	extern	LCDGoto
	extern	LCDGotoCGRAM
	extern	LCDPutChar
	extern	LCDFillChar
	extern	LCDPutHex
	extern	LCDPutUInt
	extern	LCDPutFpDX
	extern	LCDPutCStr
	extern	LCDPutStr
	extern	LCDClear

