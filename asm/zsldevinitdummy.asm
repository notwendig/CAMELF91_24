;  file zsldevinitdummy.asm
;  Dummy implementation file for opening peripheral devices.
;
;  Copyright (C) 1999-2004 by  ZiLOG, Inc.
;  All Rights Reserved.
;

	segment	CODE
	.assume adl=1
	
	XDEF _open_periphdevice
	XDEF __open_periphdevice
	XDEF _close_periphdevice
	XDEF __close_periphdevice

_open_periphdevice:
__open_periphdevice:
_close_periphdevice:
__close_periphdevice:

	ret
