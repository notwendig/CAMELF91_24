
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
	XREF __stack

ENDOFMEM EQU __stack

VERSION MACRO
	DB "1.00e (24bit) - "
ifdef _CHIPONLY
	DB 'Chiponly'
else
ifdef _DEBUG
	DB 'Debug'
else
ifdef _RELEASE	
	DB 'Release'
else
#error
endif
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

docode  EQU 0      ; flag to indicate CODE words


head    MACRO   label,name,action
		SCOPE
        DW24 link
        DB 0
link    .SET $
        DB $F-$-1,name
$$:
	xdef label
label:
		IFSAME  action,docode
		ELSE
			call action
        ENDIF
        ENDMACRO head

immed   MACRO   label,name,action
		SCOPE
        DW24 link
        DB 1
link    .SET $
        DB $F-$-1,name
$$:
	xdef label
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

 XREF	CAMEL91E_LAST
 XREF	CAMEL91D_LAST
 XREF	CAMEL91H_LAST

 XREF RESET
 XREF docolon
 XREF defuser
 XREF InpBuffer
 XREF docreate
 XREF user0
 XREF __heapbot
 XREF __heaptop
	
enddict	equ __heapbot		    ; user's code starts here	

 XREF lastword

__FORTH0 EQU __S0-PARASTACKSZ
__S0	EQU __PAD-HOLDSTACKSZ	;40
__HOLDP	EQU __PAD
__PAD	EQU	__L0-PADBUFFERSZ	; PAD buffer, 88 bytes
__L0	EQU	__R0-LEAVESTACKSZ-RETNSTACKSZ
__R0 	EQU ENDOFMEM		; Return stack, 128 B, grows down
	
 XREF		ENTRY                ;  CAMELF91E
 XREF		dbg                  ;  CAMELF91E
 XREF		EXIT                 ;  CAMELF91E
 XREF		LIT                  ;  CAMELF91E
 XREF		EXECUTE              ;  CAMELF91E
 XREF		VARIABLE             ;  CAMELF91E
 XREF		CONSTANT             ;  CAMELF91E
 XREF		docon                ;  CAMELF91E
 XREF		USER                 ;  CAMELF91E
 XREF		douser               ;  CAMELF91E
 XREF		dodoes               ;  CAMELF91E
 XREF		EMIT                 ;  CAMELF91E
 XREF		QUERYKEY             ;  CAMELF91E
 XREF		KEY                  ;  CAMELF91E
 XREF		BYE                  ;  CAMELF91E
 XREF		DUP                  ;  CAMELF91E
 XREF		QDUP                 ;  CAMELF91E
 XREF		DROP                 ;  CAMELF91E
 XREF		SWOP                 ;  CAMELF91E
 XREF		OVER                 ;  CAMELF91E
 XREF		ROT                  ;  CAMELF91E
 XREF		NIP                  ;  CAMELF91E
 XREF		TUCK                 ;  CAMELF91E
 XREF		TOR                  ;  CAMELF91E
 XREF		RFROM                ;  CAMELF91E
 XREF		RFETCH               ;  CAMELF91E
 XREF		SPFETCH              ;  CAMELF91E
 XREF		SPSTORE              ;  CAMELF91E
 XREF		RPFETCH              ;  CAMELF91E
 XREF		RPSTORE              ;  CAMELF91E
 XREF		STORE                ;  CAMELF91E
 XREF		CSTORE               ;  CAMELF91E
 XREF		FETCH                ;  CAMELF91E
 XREF		CFETCH               ;  CAMELF91E
 XREF		PSTORE               ;  CAMELF91E
 XREF		PFETCH               ;  CAMELF91E
 XREF		PCSTORE              ;  CAMELF91E
 XREF		PCFETCH              ;  CAMELF91E
 XREF		P0STORE              ;  CAMELF91E
 XREF		P0FETCH              ;  CAMELF91E
 XREF		PLUS                 ;  CAMELF91E
 XREF		MPLUS                ;  CAMELF91E
 XREF		MINUS                ;  CAMELF91E
 XREF		AND                  ;  CAMELF91E
 XREF		OR                   ;  CAMELF91E
 XREF		XOR                  ;  CAMELF91E
 XREF		INVERT               ;  CAMELF91E
 XREF		NEGATE               ;  CAMELF91E
 XREF		ONEPLUS              ;  CAMELF91E
 XREF		ONEMINUS             ;  CAMELF91E
 XREF		SWAPBYTES            ;  CAMELF91E
 XREF		TWOSTAR              ;  CAMELF91E
 XREF		TWOSLASH             ;  CAMELF91E
 XREF		LSHIFT               ;  CAMELF91E
 XREF		RSHIFT               ;  CAMELF91E
 XREF		PLUSSTORE            ;  CAMELF91E
 XREF		ZEROEQUAL            ;  CAMELF91E
 XREF		ZEROLESS             ;  CAMELF91E
 XREF		EQUAL                ;  CAMELF91E
 XREF		NOTEQUAL             ;  CAMELF91E
 XREF		LESS                 ;  CAMELF91E
 XREF		GREATER              ;  CAMELF91E
 XREF		ULESS                ;  CAMELF91E
 XREF		UGREATER             ;  CAMELF91E
 XREF		BRANCH               ;  CAMELF91E
 XREF		QBRANCH              ;  CAMELF91E
 XREF		XDO                  ;  CAMELF91E
 XREF		XLOOP                ;  CAMELF91E
 XREF		XPLUSLOOP            ;  CAMELF91E
 XREF		II                   ;  CAMELF91E
 XREF		JJ                   ;  CAMELF91E
 XREF		UNLOOP               ;  CAMELF91E
 XREF		UMSTAR               ;  CAMELF91E
 XREF		UMSLASHMOD           ;  CAMELF91E
 XREF		FILL                 ;  CAMELF91E
 XREF		CMOVE                ;  CAMELF91E
 XREF		CMOVEUP              ;  CAMELF91E
 XREF		SKIP                 ;  CAMELF91E
 XREF		SCAN                 ;  CAMELF91E
 XREF		SEQUAL               ;  CAMELF91E
 XREF		ALIGN                ;  CAMELF91D
 XREF		ALIGNED              ;  CAMELF91D
 XREF		CELL                 ;  CAMELF91D
 XREF		CELLPLUS             ;  CAMELF91D
 XREF		CELLS                ;  CAMELF91D
 XREF		CHARPLUS             ;  CAMELF91D
 XREF		CHARS                ;  CAMELF91D
 XREF		TOBODY               ;  CAMELF91D
 XREF		COMMAXT              ;  CAMELF91D
 XREF		STORECF              ;  CAMELF91D
 XREF		COMMACF              ;  CAMELF91D
 XREF		STORCOLON            ;  CAMELF91D
 XREF		CEXIT                ;  CAMELF91D
 XREF		COMMABRANCH          ;  CAMELF91D
 XREF		COMMADEST            ;  CAMELF91D
 XREF		STOREDEST            ;  CAMELF91D
 XREF		BL                   ;  CAMELF91H
 XREF		TIBSIZE              ;  CAMELF91H
 XREF		TIB                  ;  CAMELF91H
 XREF		U0                   ;  CAMELF91H
 XREF		TOIN                 ;  CAMELF91H
 XREF		BASE                 ;  CAMELF91H
 XREF		STATE                ;  CAMELF91H
 XREF		DP                   ;  CAMELF91H
 XREF		TICKSOURCE           ;  CAMELF91H
 XREF		LATEST               ;  CAMELF91H
 XREF		HP                   ;  CAMELF91H
 XREF		LP                   ;  CAMELF91H
 XREF		S0                   ;  CAMELF91H
 XREF		HOLDP                ;  CAMELF91H
 XREF		PAD                  ;  CAMELF91H
 XREF		L0                   ;  CAMELF91H
 XREF		R0                   ;  CAMELF91H
 XREF		UINIT                ;  CAMELF91H
 XREF		NINIT                ;  CAMELF91H
 XREF		MEMCFG               ;  CAMELF91H
 XREF		STOD                 ;  CAMELF91H
 XREF		QNEGATE              ;  CAMELF91H
 XREF		ABS                  ;  CAMELF91H
 XREF		DNEGATE              ;  CAMELF91H
 XREF		QDNEGATE             ;  CAMELF91H
 XREF		DABS                 ;  CAMELF91H
 XREF		MSTAR                ;  CAMELF91H
 XREF		SMSLASHREM           ;  CAMELF91H
 XREF		FMSLASHMOD           ;  CAMELF91H
 XREF		STAR                 ;  CAMELF91H
 XREF		SLASHMOD             ;  CAMELF91H
 XREF		SLASH                ;  CAMELF91H
 XREF		MODD                 ;  CAMELF91H
 XREF		SSMOD                ;  CAMELF91H
 XREF		STARSLASH            ;  CAMELF91H
 XREF		MAX                  ;  CAMELF91H
 XREF		MIN                  ;  CAMELF91H
 XREF		TWOFETCH             ;  CAMELF91H
 XREF		TWOSTORE             ;  CAMELF91H
 XREF		TWODROP              ;  CAMELF91H
 XREF		TWODUP               ;  CAMELF91H
 XREF		TWOSWAP              ;  CAMELF91H
 XREF		TWOOVER              ;  CAMELF91H
 XREF		PAGE                 ;  CAMELF91H
 XREF		COUNT                ;  CAMELF91H
 XREF		CR                   ;  CAMELF91H
 XREF		SPACE                ;  CAMELF91H
 XREF		SPACES               ;  CAMELF91H
 XREF		UMIN                 ;  CAMELF91H
 XREF		UMAX                 ;  CAMELF91H
 XREF		ACCEPT               ;  CAMELF91H
 XREF		TYPE                 ;  CAMELF91H
 XREF		XSQUOTE              ;  CAMELF91H
 XREF		SQUOTE               ;  CAMELF91H
 XREF		DOTQUOTE             ;  CAMELF91H
 XREF		UDSLASHMOD           ;  CAMELF91H
 XREF		UDSTAR               ;  CAMELF91H
 XREF		HOLD                 ;  CAMELF91H
 XREF		LESSNUM              ;  CAMELF91H
 XREF		TODIGIT              ;  CAMELF91H
 XREF		NUM                  ;  CAMELF91H
 XREF		NUMS                 ;  CAMELF91H
 XREF		NUMGREATER           ;  CAMELF91H
 XREF		SIGN                 ;  CAMELF91H
 XREF		UDOT                 ;  CAMELF91H
 XREF		DOT                  ;  CAMELF91H
 XREF		DECIMAL              ;  CAMELF91H
 XREF		HEX                  ;  CAMELF91H
 XREF		HERE                 ;  CAMELF91H
 XREF		ALLOT                ;  CAMELF91H
 XREF		COMMA                ;  CAMELF91H
 XREF		CCOMMA               ;  CAMELF91H
 XREF		SOURCE               ;  CAMELF91H
 XREF		SLASHSTRING          ;  CAMELF91H
 XREF		TOCOUNTED            ;  CAMELF91H
 XREF		WORD                 ;  CAMELF91H
 XREF		NFATOLFA             ;  CAMELF91H
 XREF		NFATOCFA             ;  CAMELF91H
 XREF		IMMEDQ               ;  CAMELF91H
 XREF		FIND                 ;  CAMELF91H
 XREF		LITERAL              ;  CAMELF91H
 XREF		DIGITQ               ;  CAMELF91H
 XREF		QSIGN                ;  CAMELF91H
 XREF		TONUMBER             ;  CAMELF91H
 XREF		QNUMBER              ;  CAMELF91H
 XREF		INTERPRET            ;  CAMELF91H
 XREF		EVALUATE             ;  CAMELF91H
 XREF		QUIT                 ;  CAMELF91H
 XREF		ABORT                ;  CAMELF91H
 XREF		QABORT               ;  CAMELF91H
 XREF		ABORTQUOTE           ;  CAMELF91H
 XREF		TICK                 ;  CAMELF91H
 XREF		CHAR                 ;  CAMELF91H
 XREF		BRACCHAR             ;  CAMELF91H
 XREF		PAREN                ;  CAMELF91H
 XREF		CREATE               ;  CAMELF91H
 XREF		XDOES                ;  CAMELF91H
 XREF		DOES                 ;  CAMELF91H
 XREF		RECURSE              ;  CAMELF91H
 XREF		LEFTBRACKET          ;  CAMELF91H
 XREF		RIGHTBRACKET         ;  CAMELF91H
 XREF		HIDE                 ;  CAMELF91H
 XREF		REVEAL               ;  CAMELF91H
 XREF		IMMEDIATE            ;  CAMELF91H
 XREF		COLON                ;  CAMELF91H
 XREF		SEMICOLON            ;  CAMELF91H
 XREF		BRACTICK             ;  CAMELF91H
 XREF		POSTPONE             ;  CAMELF91H
 XREF		IFF                  ;  CAMELF91H
 XREF		THEN                 ;  CAMELF91H
 XREF		ELLSE                ;  CAMELF91H
 XREF		BEGIN                ;  CAMELF91H
 XREF		UNTIL                ;  CAMELF91H
 XREF		AGAIN                ;  CAMELF91H
 XREF		WHILE                ;  CAMELF91H
 XREF		REPEAT               ;  CAMELF91H
 XREF		TOL                  ;  CAMELF91H
 XREF		LFROM                ;  CAMELF91H
 XREF		DO                   ;  CAMELF91H
 XREF		ENDLOOP              ;  CAMELF91H
 XREF		LOOP                 ;  CAMELF91H
 XREF		PLUSLOOP             ;  CAMELF91H
 XREF		LEAVE                ;  CAMELF91H
 XREF		WITHIN               ;  CAMELF91H
 XREF		MOVE                 ;  CAMELF91H
 XREF		DEPTH                ;  CAMELF91H
 XREF		ENVIRONMENTQ         ;  CAMELF91H
 XREF		WORDS                ;  CAMELF91H
 XREF		DOTS                 ;  CAMELF91H
 XREF		COLD                 ;  CAMELF91H
 XREF		DDOTR                ;  CAMELF91H
 XREF		DOTR                 ;  CAMELF91H
 XREF		DUMP                 ;  CAMELF91H
