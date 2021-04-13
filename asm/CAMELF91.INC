
;CAMLF91 -- Forth Kernel
; CamelForth for the Zilog eZ80F91-24bit
; portions (C) 2021 for eZ80F91 24bit by JSievers@NadiSoft.de
; ..based on CamelForth for the Zilog eZ80F91 16bit
; (C) 2004 Douglas Beattie Jr.

; Permission is granted to freely copy, modify,
; and distribute this program for personal or
; educational use.  Commercial inquiries should
; be directed to the author at 115 First St.,
; #105, Collingwood, Ontario L9Y 4W3, Canada
;
VERSION MACRO 
	DB "1.00e (24bit) - " 
ifdef CHIPONLY
ENDOFMEM	EQU B7FFFFh		; ROM=ChipFlash 000000-03FFFF, SRAM=on chip B7D000-B7FFFF
	DB "Chiponly"
else
ifdef _DEBUG
ENDOFMEM	EQU BFFFFFh		; ROM=ModulSRAM 000000-07FFFF, SRAM (DevBord+Chip) B7D000-BFFFFF
	DB "Debug"
else
ENDOFMEM	EQU C7FFFFh		; ROM=ChipFlash 000000-03FFFF, SRAM (Module+DevBord+Chip) B7D000-C7FFFF
	DB "Release"
endif
endif
	DB ", 13 APR 2021",0Dh,0Ah
	ENDMACRO

; frequency must be set properly for baud rate to
; be initialized correctly.
	xref _SYS_CLK_FREQ 
	; void * _set_vector(unsigned int vector, void(*handler)(void));
	xref _set_vector
	
; ********************************************************
; *** SET UP SERIAL COMMUNICATIONS WITH HOST.
; *** UART0
;
BAUDRATE EQU 115200 ;57600 ;38400
DIVISOR EQU ((_SYS_CLK_FREQ / BAUDRATE) / 16)

;************************************************************************
; ===============================================
; User ROM struct and memoriy config.
; Macros to define Forth headers
; HEAD  label,name,action
; IMMED label,name,action
;    label  = assembler name for this word
;             (special characters not allowed)
;    name   = Forth's name for this word
;    action = code routine for this word, e.g.
;             DOCOLON, or DOCODE for code words
; IMMED defines a header for an IMMEDIATE word.
;
; NOTE: length of name field is now automatically
; calculated by the macro, and is no longer a parameter

; Add User-area
; 01 APR 2021 v1.00e-24bit for eZ80F91 JSievers@NadiSoft.de

USERAREA	STRUCT
U0 			DS	3		;  0 USER U0        current user area addres
TOIN 		DS	3		;  3 USER >IN       holds offset into TIB
BASE		DS	3		;  6 USER BASE      holds conversion radix
STATE		DS	3		;  9 USER STATE     holds compiler state
DP			DS	3		; 12 USER DP        holds dictionary ptr
TICKSOURCE 	DS	6		; 15 USER SOURCE    two cells: len, adrs
LATEST		DS	3		; 21 USER LATEST    last word in dict.
HP			DS	3		; 24 USER HP        HOLD pointer
LP			DS	3		; 28 USER LP        Lreave-stack pointer
S0			DS	3		; 31 Parameter stack, grows down
HOLDP		DS  3		; 34 HOLD grows down
PAD			DS  3		; 37 PAD buffer grows up
L0         	DS  3		; 40 bottom of Leave stack grows up
R0         	DS  3		; 43 Return stack, grows down
USERAREASZ 	ENDSTRUCT USERAREA

PARASTACKSZ	EQU		256
HOLDSTACKSZ	EQU 	 80
PADBUFFERSZ	EQU		 88
LEAVESTACKSZ EQU 	128
RETNSTACKSZ	EQU 	256

USERSEGMENT EQU	ENDOFMEM - PARASTACKSZ - HOLDSTACKSZ - PADBUFFERSZ - LEAVESTACKSZ - RETNSTACKSZ +1

docode  EQU 0      ; flag to indicate CODE words
link    .SET 0     ; link to previous Forth word


head    MACRO   label,name,action
        DW24 link
        DB 0
link    .SET $
        DB $F-$-1,name
$$:
label:
		IFSAME  action,docode
		ELSE
			call action
        ENDIF
        ENDMACRO head

immed   MACRO   label,name,action
        DW24 link
        DB 1
link    .SET $
        DB $F-$-1,name
$$:
label:
		IFSAME  action,docode
		ELSE
			call action
        ENDIF
        ENDMACRO immed

; The NEXT macro (7 bytes) assembles the 'next'
; code in-line in every eZ80 CamelForth CODE word.
next    MACRO
		EX DE,HL
		nexthl
        ENDMACRO next

; NEXTHL is used when the IP is already in HL.
nexthl  MACRO
		LD DE,(HL)
        EX DE,HL
		INC DE
		INC DE
		INC	DE
        JP (HL)
        ENDMACRO nexthl

	xref __heaptop

