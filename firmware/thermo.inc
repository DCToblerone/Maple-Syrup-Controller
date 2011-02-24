;----------------------------------------------------------------------
; thermo.inc
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
; The Original Code is the thermometer include.
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

ifdef THERMO_ASM
	global	ThermoInit
	global	ThermoThread
	global	pumpOut
	global	thermoOut
	global	eGals
else
	extern	ThermoInit
	extern	ThermoThread
	extern	pumpOut
	extern	thermoOut
	extern	eGals	; so we can clear it on init!
endif

; set out timeout value to 7.5 miliseconds, this ought to be
; ample time for the capacitor to discharge
; DMC: Make this time unusually large so we can run other threads more
#define DischargeTime 75000

