;CAMLF91d.S -- Forth Kernel
; CamelForth for the Zilog eZ80F91-24bit
; portions (C) 2021 for eZ80F91 24bit by JSievers@NadiSoft.de
; ..based on CamelForth for the Zilog eZ80F91 16bit
; (C) 2004 Douglas Beattie Jr.
; ..based on CamelForth for the Zilog Z80
; (C) 1994 Bradford J. Rodriguez
; Permission is granted to freely copy, modify,
; and distribute this program for personal or
; educational use.  Commercial inquiries should
; be directed to the author at 115 First St.,
; #105, Collingwood, Ontario L9Y 4W3, Canada
;
; CAMLF91d.S: CPU and Model Dependencies
;   Source code is for the Zilog Macro Assembler.
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
;   Word names in upper case are from the ANS
;   Forth Core word set.  Names in lower case are
;   "internal" implementation words & extensions.
;
; Direct-Threaded Forth model for Zilog eZ80
;   cell size is   24 bits (3 bytes)
;   char size is    8 bits (1 byte)
;   address unit is 8 bits (1 byte), i.e.,
;       addresses are byte-aligned.
; ===============================================

; ALIGNMENT AND PORTABILITY OPERATORS ===========
; Many of these are synonyms for other words,
; and so are defined as CODE words.

	DEFINE CAMELF91D,SPACE=rom
	SEGMENT CAMELF91D
	.ASSUME ADL=1
	
ifdef INCLUDED

;C ALIGN    --                         align HERE
    head ALIGN,{"ALIGN"},docode
noop:   next

;C ALIGNED  addr -- a-addr       align given addr
    head ALIGNED,{"ALIGNED"},docode
        jr noop

;Z CELL     -- n                 size of one cell
    head CELL,{"CELL"},docon
        dw24 3

;C CELL+    a-addr1 -- a-addr2      add cell size
;   2 + ;
    head CELLPLUS,{"CELL+"},docode
        inc bc
        inc bc
		inc bc
        next

;C CELLS    n1 -- n2            cells->adrs units
    head CELLS,{"CELLS"},docode
		ld	hl,bc
		add	hl,bc
		add hl,bc
		ld	bc,hl
		next
        ;jp TWOSTAR

;C CHAR+    c-addr1 -- c-addr2   add char size
    head CHARPLUS,{"CHAR+"},docode
        jp ONEPLUS

;C CHARS    n1 -- n2            chars->adrs units
    head CHARS,{"CHARS"},docode
        jr noop

;C >BODY    xt -- a-addr      adrs of param field
;   4 + ;                     eZ80 (4 byte CALL)
    head TOBODY,{">BODY"},docolon
        DW24 LIT,4,PLUS,EXIT

;X COMPILE,  xt --         append execution token
; I called this word ,XT before I discovered that
; it is defined in the ANSI standard as COMPILE,.
; On a DTC Forth this simply appends xt (like , )
; but on an STC Forth this must append 'CALL xt'.
    head COMMAXT,{"COMPILE,"},docode
        jp COMMA

;Z !CF    adrs cfa --   set code action of a word
;   0CD OVER C!         store 'CALL adrs' instr
;   1+ ! ;              eZ80 VERSION
; Depending on the implementation this could
; append CALL adrs or JUMP adrs.
    head STORECF,{"!CF"},docolon
        DW24 LIT,0CDH,OVER,CSTORE
        DW24 ONEPLUS,STORE,EXIT

;Z ,CF    adrs --       append a code field
;   HERE !CF 3 ALLOT ;  Z80 VERSION (3 bytes)
    head COMMACF,{",CF"},docolon
        DW24 HERE,STORECF,LIT,4,ALLOT,EXIT

;Z !COLON   --      change code field to docolon
;   -3 ALLOT docolon-adrs ,CF ;
; This should be used immediately after CREATE.
; This is made a distinct word, because on an STC
; Forth, colon definitions have no code field.
    head STORCOLON,{"!COLON"},docolon
        DW24 LIT,-4,ALLOT
        DW24 LIT,docolon,COMMACF,EXIT

;Z ,EXIT    --      append hi-level EXIT action
;   ['] EXIT ,XT ;
; This is made a distinct word, because on an STC
; Forth, it appends a RET instruction, not an xt.
    head CEXIT,{",EXIT"},docolon
        DW24 LIT,EXIT,COMMAXT,EXIT

; CONTROL STRUCTURES ============================
; These words allow Forth control structure words
; to be defined portably.

;Z ,BRANCH   xt --    append a branch instruction
; xt is the branch operator to use, e.g. qbranch
; or (loop).  It does NOT append the destination
; address.  On the Z80 this is equivalent to ,XT.
    head COMMABRANCH,{",BRANCH"},docode
        jp COMMA

;Z ,DEST   dest --        append a branch address
; This appends the given destination address to
; the branch instruction.  On the Z80 this is ","
; ...other CPUs may use relative addressing.
    head COMMADEST,{",DEST"},docode
        jp COMMA

;Z !DEST   dest adrs --    change a branch dest'n
; Changes the destination address found at 'adrs'
; to the given 'dest'.  On the Z80 this is "!"
; ...other CPUs may need relative addressing.
    head STOREDEST,{"!DEST"},docode
        jp STORE

else
	ds 1	; makes the relist happy
endif ;INCLUDED
