
;CAMLF91 -- Forth Kernel
; CamelForth for the Zilog eZ80F91
; portions (C) 2004 Douglas Beattie Jr.
;
; ..based on CamelForth for the Zilog Z80
; (C) 1994 Bradford J. Rodriguez
; Permission is granted to freely copy, modify,
; and distribute this program for personal or
; educational use.  Commercial inquiries should
; be directed to the author at 115 First St.,
; #105, Collingwood, Ontario L9Y 4W3, Canada
;

; frequency must be set properly for baud rate to
; be initialized correctly.
	xref _SYS_CLK_FREQ ;EQU 50000000	;F91 Development Kit, 20_MHz

; ********************************************************
; *** SET UP SERIAL COMMUNICATIONS WITH HOST.
; *** UART0
;
BAUDRATE EQU 115200 ;57600 ;38400
DIVISOR EQU ((_SYS_CLK_FREQ / BAUDRATE) / 16)
; RS-232 TERMINAL I/O  =============================

UART_THRE   EQU 20h     ;bit mask for okay to Tx
UART_DR     EQU 01h     ;bit mask for Rx'd char ready

;************************************************************************
; ===============================================
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
;

USERAREA	STRUCT
U0 			DS	3		;  0 USER U0        current user area adrs
TOIN 		DS	3		;  3 USER >IN       holds offset into TIB
BASE		DS	3		;  6 USER BASE      holds conversion radix
STATE		DS	3		;  9 USER STATE     holds compiler state
DP			DS	3		; 12 USER DP        holds dictionary ptr
TICKSOURCE 	DS	6		; 15 USER SOURCE    two cells: len, adrs
LATEST		DS	3		; 21 USER LATEST    last word in dict.
HP			DS	3		; 24 USER HP        HOLD pointer
LP			DS	3		; 28 USER LP        Lreave-stack pointer
USERAREASZ 	ENDSTRUCT USERAREA

PARASTACKSZ	EQU	256		;128
HOLDSTACKSZ	EQU  80		;40
PADBUFFERSZ	EQU	 88
LEAVESTACKSZ EQU 128	;97
RETNSTACKSZ	EQU 256		;128

USERSEGMENT EQU	ENDOFMEM - USERAREASZ - PARASTACKSZ - HOLDSTACKSZ - PADBUFFERSZ - LEAVESTACKSZ - RETNSTACKSZ + 1

docode  EQU 0      ; flag to indicate CODE words
link    .SET 0     ; link to previous Forth word

trace	MACRO	action
if 0	
		push 	bc
		push	hl
		ld		c,0dh
		CALL    TXMCHR	
		ld		c,0ah
		CALL    TXMCHR
		ld		a,(tracelvl)
		ld		b,a
		IFSAME  action,docolon		
		inc		a
		ld		(tracelvl),a
		endif
		ld		c,'.'
		jr		$F
	
$lvl:	CALL    TXMCHR
$$:		djnz	$lvl
		ld		hl,link
		ld		b,(hl)
$$:		inc 	hl
		ld		c,(hl)
		CALL    TXMCHR
		djnz	$B	
		ld		c,20h
		CALL    TXMCHR
		pop		hl
		pop		bc
endif		
		ENDMAC
	
tracex	MACRO	
if 0	
		ld		a,(tracelvl)
		or		a
		jr		z,$F
		dec		a
		ld		(tracelvl),a
$$:
endif	
		ENDMAC
	
head    MACRO   label,name,action
        DW24 link
        DB 0
link    .SET $
;;      D B &length,&name
        DB $F-$-1,name
$$:
		xdef label
label:	trace action
		IFSAME  action,docode
		ELSE
			call action
        ENDIF
        ENDMACRO head

immed   MACRO   label,name,action
        DW24 link
        DB 1
link    .SET $
;;      DB length,&name
        DB $F-$-1,name
$$:
		xdef label
label:	trace action
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
ENDOFMEM EQU 7FFFFh


