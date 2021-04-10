;CAMLF91h.S -- Forth Kernel
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
;
; CAMLF91h.S: High Level Words
;   Source code is for the Zilog Macro Assembler.
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
;   Word names in upper case are from the ANS
;   Forth Core word set.  Names in lower case are
;   "internal" implementation words & extensions.
; ===============================================

; SYSTEM VARIABLES & CONSTANTS ==================
	
	DEFINE CAMELF91H,SPACE=rom
	SEGMENT CAMELF91H
	.ASSUME ADL=1
	
ifdef INCLUDED

;C BL      -- char            an ASCII space
    head BL,{"BL"},docon
        DW24 20h

;Z tibsize  -- n         size of TIB
    head TIBSIZE,{"TIBSIZE"},docon
        DW24 124          ; 2 chars safety zone

;X tib     -- a-addr     Terminal Input Buffer
;  HEX 82 CONSTANT TIB   CP/M systems: 126 bytes
;  HEX -80 USER TIB      others: below user area
    head TIB,{"TIB"},docon
        DW24 InpBuffer

;Z u0      -- a-addr       current user area adrs
;  0 USER U0
    head U0,{"U0"},douser
        DW24	USERAREA.U0	;0

;C >IN     -- a-addr        holds offset into TIB
;  2 USER >IN
    head TOIN,{">IN"},douser
        DW24 USERAREA.TOIN	;3
  
;C BASE    -- a-addr       holds conversion radix
;  4 USER BASE
    head BASE,{"BASE"},douser
        DW24 USERAREA.BASE	;6

;C STATE   -- a-addr       holds compiler state
;  6 USER STATE
    head STATE,{"STATE"},douser
        DW24 USERAREA.STATE	;9

;Z dp      -- a-addr       holds dictionary ptr
;  8 USER DP
    head DP,{"DP"},douser
        DW24 USERAREA.DP	;12

;Z 'source  -- a-addr      two cells: len, adrs
;  10 USER 'SOURCE
    head TICKSOURCE,{"\'SOURCE"},douser
        DW24 USERAREA.TICKSOURCE	;15

;Z latest    -- a-addr     last word in dict.
;   14 USER LATEST
    head LATEST,{"LATEST"},douser
        DW24 USERAREA.LATEST	;18

;Z hp       -- a-addr     HOLD pointer
;   16 USER HP
    head HP,{"HP"},douser
        DW24 USERAREA.HP	;21

;Z LP       -- a-addr     Leave-stack pointer
;   18 USER LP
    head LP,{"LP"},douser
        DW24 USERAREA.LP	;24

;Z s0       -- a-addr     end of parameter stack
    head S0,{"S0"},douser
        DW24 USERAREA.S0	;100h

;X HOLDP	-- a-addr    user HOLDP buffer
;                         = start of hold area!
    head HOLDP,{"HOLDP"},douser
        DW24 USERAREA.HOLDP	;128h

;X PAD       -- a-addr    user PAD buffer
;                         = end of hold area!
    head PAD,{"PAD"},douser
        DW24 USERAREA.PAD	;128h

;Z l0       -- a-addr     bottom of Leave stack
    head L0,{"L0"},douser
        DW24 USERAREA.L0	;180h

;Z r0       -- a-addr     end of return stack
    head R0,{"R0"},douser
        DW24 USERAREA.R0	;200h

;Z uinit    -- addr  initial values for user area
    head UINIT,{"UINIT"},docreate
defuser	.tag USERAREA
defuser:
def_U0 			DW24 user		;  0 USER U0        current user area adrs
def_TOIN 		DW24 0			;  3 USER >IN       holds offset into TIB
def_BASE		DW24 10			;  6 USER BASE      holds conversion radix
def_STATE		DW24 0			;  9 USER STATE     holds compiler state
def_DP			DW24 enddict	; 12 USER DP        holds dictionary ptr
def_TICKSOURCE 	DW24 0,0		; 15 USER SOURCE    two cells: len, adrs
def_LATEST		DW24 lastword 	; 21 USER LATEST    last word in dict.
def_HP			DW24 __HOLDP	; 24 USER HP        HOLD pointer
def_LP			DW24 __L0		; 28 USER LP        Lreave-stack pointer
def_S0			DW24 __S0		; 31 Parameter stack, grows down
def_HOLDP		DW24 __HOLDP	; 34 HOLD grows down
def_PAD			DW24 __PAD		; 37 PAD buffer grows up
def_L0         	DW24 __L0		; 40 bottom of Leave stack grows up
def_R0         	DW24 __R0		; 43 Return stack, grows down


;Z #init    -- n    #bytes of user area init data
    head NINIT,{"#INIT"},docon
        DW24 USERAREASZ

;ifdef _DEBUG
	head MEMCFG,{"MEMCFG"},docolon
		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, "TIB:       "
$$      DW24 TYPE
		DW24 HEX,TIB,UDOT,DECIMAL,TIBSIZE,UDOT

		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, "U0:        "
$$      DW24 TYPE
		DW24 HEX,U0,DUP,UDOT,FETCH,UDOT
		
		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, ">IN:       "
$$      DW24 TYPE
		DW24 HEX,TOIN,DUP,UDOT,DECIMAL,FETCH,UDOT
		
		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, "BASE:      "
$$      DW24 TYPE
		DW24 HEX,BASE,DUP,UDOT,DECIMAL,FETCH,UDOT
		
		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, "STATE:     "
$$      DW24 TYPE
		DW24 HEX,STATE,DUP,UDOT,FETCH,UDOT
		
		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, "DP:        "
$$      DW24 TYPE
		DW24 HEX,DP,DUP,UDOT,FETCH,UDOT

		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, "SOURCE:    "
$$      DW24 TYPE
		DW24 HEX,TICKSOURCE,DUP,UDOT,TWOFETCH,UDOT,UDOT

		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, "LATEST:    "
$$      DW24 TYPE
		DW24 HEX,LATEST,DUP,UDOT,FETCH,UDOT

		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, "HP:        "
$$      DW24 TYPE
		DW24 HEX,HP,DUP,UDOT,FETCH,UDOT

		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, "LP:        "
$$      DW24 TYPE
		DW24 HEX,LP,DUP,UDOT,FETCH,UDOT

		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, "S0:        "
$$      DW24 TYPE
		DW24 HEX,S0,DUP,UDOT,FETCH,UDOT

		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, "HOLD:      "
$$      DW24 TYPE
		DW24 HEX,HOLDP,DUP,UDOT,FETCH,UDOT

		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, "PAD:       "
$$      DW24 TYPE
		DW24 HEX,PAD,DUP,UDOT,FETCH,UDOT

		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, "L0:        "
$$      DW24 TYPE
		DW24 HEX,L0,DUP,UDOT,FETCH,UDOT

		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah, "R0:        "
$$      DW24 TYPE
		DW24 HEX,R0,DUP,UDOT,FETCH,UDOT

		DW24 XSQUOTE
        DB $F-$-1,0Dh,0Ah
$$      DW24 TYPE,EXIT
;endif	

; ARITHMETIC OPERATORS ==========================

;C S>D    n -- d          single -> double prec.
;   DUP 0< ;
    head STOD,{"S>D"},docolon
        DW24 DUP,ZEROLESS,EXIT

;Z ?NEGATE  n1 n2 -- n3  negate n1 if n2 negative
;   0< IF NEGATE THEN ;        ...a common factor
    head QNEGATE,{"?NEGATE"},docolon
        DW24 ZEROLESS,QBRANCH,QNEG1,NEGATE
QNEG1:  DW24 EXIT

;C ABS     n1 -- +n2     absolute value
;   DUP ?NEGATE ;
    head ABS,{"ABS"},docolon
        DW24 DUP,QNEGATE,EXIT

;X DNEGATE   d1 -- d2     negate double precision
;   SWAP INVERT SWAP INVERT 1 M+ ;
    head DNEGATE,{"DNEGATE"},docolon
        DW24 SWOP,INVERT,SWOP,INVERT,LIT,1,MPLUS
        DW24 EXIT

;Z ?DNEGATE  d1 n -- d2   negate d1 if n negative
;   0< IF DNEGATE THEN ;       ...a common factor
    head QDNEGATE,{"?DNEGATE"},docolon
        DW24 ZEROLESS,QBRANCH,DNEG1,DNEGATE
DNEG1:  DW24 EXIT

;X DABS     d1 -- +d2    absolute value dbl.prec.
;   DUP ?DNEGATE ;
    head DABS,{"DABS"},docolon
        DW24 DUP,QDNEGATE,EXIT

;C M*     n1 n2 -- d    signed 16*16->32 multiply
;   2DUP XOR >R        carries sign of the result
;   SWAP ABS SWAP ABS UM*
;   R> ?DNEGATE ;
    head MSTAR,{"M*"},docolon
        DW24 TWODUP,XOR,TOR
        DW24 SWOP,ABS,SWOP,ABS,UMSTAR
        DW24 RFROM,QDNEGATE,EXIT

;C SM/REM   d1 n1 -- n2 n3   symmetric signed div
;   2DUP XOR >R              sign of quotient
;   OVER >R                  sign of remainder
;   ABS >R DABS R> UM/MOD
;   SWAP R> ?NEGATE
;   SWAP R> ?NEGATE ;
; Ref. dpANS-6 section 3.2.2.1.
    head SMSLASHREM,{"SM/REM"},docolon
        DW24 TWODUP,XOR,TOR,OVER,TOR
        DW24 ABS,TOR,DABS,RFROM,UMSLASHMOD
        DW24 SWOP,RFROM,QNEGATE,SWOP,RFROM,QNEGATE
        DW24 EXIT

;C FM/MOD   d1 n1 -- n2 n3   floored signed div'n
;   DUP >R              save divisor
;   SM/REM
;   DUP 0< IF           if quotient negative,
;       SWAP R> +         add divisor to rem'dr
;       SWAP 1-           decrement quotient
;   ELSE R> DROP THEN ;
; Ref. dpANS-6 section 3.2.2.1.
    head FMSLASHMOD,{"FM/MOD"},docolon
        DW24 DUP,TOR,SMSLASHREM
        DW24 DUP,ZEROLESS,QBRANCH,FMMOD1
        DW24 SWOP,RFROM,PLUS,SWOP,ONEMINUS
        DW24 BRANCH,FMMOD2
FMMOD1: DW24 RFROM,DROP
FMMOD2: DW24 EXIT

;C *      n1 n2 -- n3       signed multiply
;   M* DROP ;
    head STAR,{"*"},docolon
        DW24 MSTAR,DROP,EXIT

;C /MOD   n1 n2 -- n3 n4    signed divide/rem'dr
;   >R S>D R> FM/MOD ;
    head SLASHMOD,{"/MOD"},docolon
        DW24 TOR,STOD,RFROM,FMSLASHMOD,EXIT

;C /      n1 n2 -- n3       signed divide
;   /MOD nip ;
    head SLASH,{"/"},docolon
        DW24 SLASHMOD,NIP,EXIT

;C MOD    n1 n2 -- n3       signed remainder
;   /MOD DROP ;
    head {MODD},{"MOD"},docolon
        DW24 SLASHMOD,DROP,EXIT

;C */MOD  n1 n2 n3 -- n4 n5    n1*n2/n3, rem&quot
;   >R M* R> FM/MOD ;
    head SSMOD,{"*/MOD"},docolon
        DW24 TOR,MSTAR,RFROM,FMSLASHMOD,EXIT

;C */     n1 n2 n3 -- n4        n1*n2/n3
;   */MOD nip ;
    head STARSLASH,{"*/"},docolon
        DW24 SSMOD,NIP,EXIT

;C MAX    n1 n2 -- n3       signed maximum
;   2DUP < IF SWAP THEN DROP ;
    head MAX,{"MAX"},docolon
        DW24 TWODUP,LESS,QBRANCH,MAX1,SWOP
MAX1:   DW24 DROP,EXIT

;C MIN    n1 n2 -- n3       signed minimum
;   2DUP > IF SWAP THEN DROP ;
    head MIN,{"MIN"},docolon
        DW24 TWODUP,GREATER,QBRANCH,MIN1,SWOP
MIN1:   DW24 DROP,EXIT

; DOUBLE OPERATORS ==============================

;C 2@    a-addr -- x1 x2    fetch 2 cells
;   DUP CELL+ @ SWAP @ ;
;   the lower address will appear on top of stack
    head TWOFETCH,{"2@"},docolon
        DW24 DUP,CELLPLUS,FETCH,SWOP,FETCH,EXIT

;C 2!    x1 x2 a-addr --    store 2 cells
;   SWAP OVER ! CELL+ ! ;
;   the top of stack is stored at the lower adrs
    head TWOSTORE,{"2!"},docolon
        DW24 SWOP,OVER,STORE,CELLPLUS,STORE,EXIT

;C 2DROP  x1 x2 --          drop 2 cells
;   DROP DROP ;
    head TWODROP,{"2DROP"},docolon
        DW24 DROP,DROP,EXIT

;C 2DUP   x1 x2 -- x1 x2 x1 x2   dup top 2 cells
;   OVER OVER ;
    head TWODUP,{"2DUP"},docolon
        DW24 OVER,OVER,EXIT

;C 2SWAP  x1 x2 x3 x4 -- x3 x4 x1 x2  per diagram
;   ROT >R ROT R> ;
    head TWOSWAP,{"2SWAP"},docolon
        DW24 ROT,TOR,ROT,RFROM,EXIT

;C 2OVER  x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2
;   >R >R 2DUP R> R> 2SWAP ;
    head TWOOVER,{"2OVER"},docolon
        DW24 TOR,TOR,TWODUP,RFROM,RFROM
        DW24 TWOSWAP,EXIT

; INPUT/OUTPUT ==================================

;C PAGE      --   Clear display and home cursor (VT-100)
; { 1B 5B 32 4A } = ESC [2J
    head PAGE,{"PAGE"},docolon
        DW24 LIT,1Bh,EMIT
        DW24 LIT,5Bh,EMIT
        DW24 LIT,32h,EMIT
        DW24 LIT,4Ah,EMIT
        DW24  EXIT

;C COUNT   c-addr1 -- c-addr2 u  counted->adr/len
;   DUP CHAR+ SWAP C@ ;
    head COUNT,{"COUNT"},docolon
        DW24 DUP,CHARPLUS,SWOP,CFETCH,EXIT

;C CR      --               output newline
;   0D EMIT 0A EMIT ;
    head CR,{"CR"},docolon
        DW24 LIT,0Dh,EMIT,LIT,0Ah,EMIT,EXIT

;C SPACE   --               output a space
;   BL EMIT ;
    head SPACE,{"SPACE"},docolon
        DW24 BL,EMIT,EXIT

;C SPACES   n --            output n spaces
;   BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
    head SPACES,{"SPACES"},docolon
SPCS1:  DW24 DUP,QBRANCH,SPCS2
        DW24 SPACE,ONEMINUS,BRANCH,SPCS1
SPCS2:  DW24 DROP,EXIT

;Z umin     u1 u2 -- u      unsigned minimum
;   2DUP U> IF SWAP THEN DROP ;
    head UMIN,{"UMIN"},docolon
        DW24 TWODUP,UGREATER,QBRANCH,UMIN1,SWOP
UMIN1:  DW24 DROP,EXIT

;Z umax    u1 u2 -- u       unsigned maximum
;   2DUP U< IF SWAP THEN DROP ;
    head UMAX,{"UMAX"},docolon
        DW24 TWODUP,ULESS,QBRANCH,UMAX1,SWOP
UMAX1:  DW24 DROP,EXIT

;C ACCEPT  c-addr +n -- +n'  get line from term'l
;   OVER + 1- OVER      -- sa ea a
;   BEGIN KEY           -- sa ea a c
;   DUP 0D <> WHILE
;       DUP EMIT        -- sa ea a c
;       DUP 8 = IF  DROP 1-    >R OVER R> UMAX
;             ELSE  OVER C! 1+ OVER UMIN
;       THEN            -- sa ea a
;   REPEAT              -- sa ea a c
;   DROP NIP SWAP - ;
    head ACCEPT,{"ACCEPT"},docolon
        DW24 OVER,PLUS,ONEMINUS,OVER
ACC1:   DW24 KEY,DUP,LIT,0DH,NOTEQUAL,QBRANCH,ACC5
        DW24 DUP,EMIT,DUP,LIT,8,EQUAL,QBRANCH,ACC3
        DW24 DROP,ONEMINUS,TOR,OVER,RFROM,UMAX
        DW24 BRANCH,ACC4
ACC3:   DW24 OVER,CSTORE,ONEPLUS,OVER,UMIN
ACC4:   DW24 BRANCH,ACC1
ACC5:   DW24 DROP,NIP,SWOP,MINUS,EXIT

;C TYPE    c-addr +n --     type line to term'l
;   ?DUP IF
;     OVER + SWAP DO I C@ EMIT LOOP
;   ELSE DROP THEN ;
    head TYPE,{"TYPE"},docolon
        DW24 QDUP,QBRANCH,TYP4
        DW24 OVER,PLUS,SWOP,XDO
TYP3:   DW24 II,CFETCH,EMIT,XLOOP,TYP3
        DW24 BRANCH,TYP5
TYP4:   DW24 DROP
TYP5:   DW24 EXIT

;Z (S")     -- c-addr u   run-time code for S"
;   R> COUNT 2DUP + ALIGNED >R  ;
    head XSQUOTE,{"(S\")"},docolon
        DW24 RFROM,COUNT,TWODUP,PLUS,ALIGNED,TOR
        DW24 EXIT

;C S"       --         compile in-line string
;   COMPILE (S")  [ HEX ]
;   22 WORD C@ 1+ ALIGNED ALLOT ; IMMEDIATE
    immed SQUOTE,{"S\""},docolon
        DW24 LIT,XSQUOTE,COMMAXT
        DW24 LIT,22H,WORD,CFETCH,ONEPLUS
        DW24 ALIGNED,ALLOT,EXIT

;C ."       --         compile string to print
;   POSTPONE S"  POSTPONE TYPE ; IMMEDIATE
    immed DOTQUOTE,{".\""},docolon
        DW24 SQUOTE
        DW24 LIT,TYPE,COMMAXT
        DW24 EXIT

; NUMERIC OUTPUT ================================
; Numeric conversion is done l.s.digit first, so
; the output buffer is built backwards in memory.

; Some double-precision arithmetic operators are
; needed to implement ANSI numeric conversion.

;Z UD/MOD   ud1 u2 -- u3 ud4   32/16->32 divide
;   >R 0 R@ UM/MOD  ROT ROT R> UM/MOD ROT ;
    head UDSLASHMOD,{"UD/MOD"},docolon
        DW24 TOR,LIT,0,RFETCH,UMSLASHMOD,ROT,ROT
        DW24 RFROM,UMSLASHMOD,ROT,EXIT

;Z UD*      ud1 d2 -- ud3      32*16->32 multiply
;   DUP >R UM* DROP  SWAP R> UM* ROT + ;
    head UDSTAR,{"UD*"},docolon
        DW24 DUP,TOR,UMSTAR,DROP
        DW24 SWOP,RFROM,UMSTAR,ROT,PLUS,EXIT

;C HOLD  char --        add char to output string
;   -1 HP +!  HP @ C! ;
    head HOLD,{"HOLD"},docolon
        DW24 LIT,-1,HP,PLUSSTORE
        DW24 HP,FETCH,CSTORE,EXIT

;C <#    --             begin numeric conversion
;   PAD HP ! ;          (initialize Hold Pointer)
    head LESSNUM,{"<#"},docolon
        DW24 HOLDP,FETCH,HP,STORE,EXIT

;Z >digit   n -- c      convert to 0..9A..Z
;   [ HEX ] DUP 9 > 7 AND + 30 + ;
    head TODIGIT,{">DIGIT"},docolon
        DW24 DUP,LIT,9,GREATER,LIT,7,AND,PLUS
        DW24 LIT,30H,PLUS,EXIT

;C #     ud1 -- ud2     convert 1 digit of output
;   BASE @ UD/MOD ROT >digit HOLD ;
    head NUM,{"#"},docolon
        DW24 BASE,FETCH,UDSLASHMOD,ROT,TODIGIT
        DW24 HOLD,EXIT

;C #S    ud1 -- ud2     convert remaining digits
;   BEGIN # 2DUP OR 0= UNTIL ;
    head NUMS,{"#S"},docolon
NUMS1:  DW24 NUM,TWODUP,OR,ZEROEQUAL,QBRANCH,NUMS1
        DW24 EXIT

;C #>    ud1 -- c-addr u    end conv., get string
;   2DROP HP @ PAD OVER - ;
    head NUMGREATER,{"#>"},docolon
        DW24 TWODROP,HP,FETCH,HOLDP,FETCH,OVER,MINUS,EXIT

;C SIGN  n --           add minus sign if n<0
;   0< IF 2D HOLD THEN ;
    head SIGN,{"SIGN"},docolon
        DW24 ZEROLESS,QBRANCH,SIGN1,LIT,2DH,HOLD
SIGN1:  DW24 EXIT

;C U.    u --           display u unsigned
;   <# 0 #S #> TYPE SPACE ;
    head UDOT,{"U."},docolon
        DW24 LESSNUM,LIT,0,NUMS,NUMGREATER,TYPE
        DW24 SPACE,EXIT

;C .     n --           display n signed
;   <# DUP ABS 0 #S ROT SIGN #> TYPE SPACE ;
    head DOT,{"."},docolon
        DW24 LESSNUM,DUP,ABS,LIT,0,NUMS
        DW24 ROT,SIGN,NUMGREATER,TYPE,SPACE,EXIT

;C DECIMAL  --      set number base to decimal
;   10 BASE ! ;
    head DECIMAL,{"DECIMAL"},docolon
        DW24 LIT,10,BASE,STORE,EXIT

;X HEX     --       set number base to hex
;   16 BASE ! ;
    head HEX,{"HEX"},docolon
        DW24 LIT,16,BASE,STORE,EXIT

; DICTIONARY MANAGEMENT =========================

;C HERE    -- addr      returns dictionary ptr
;   DP @ ;
    head HERE,{"HERE"},docolon
        DW24 DP,FETCH,EXIT

;C ALLOT   n --         allocate n bytes in dict
;   DP +! ;
    head ALLOT,{"ALLOT"},docolon
        DW24 DP,PLUSSTORE,EXIT

; Note: , and C, are only valid for combined
; Code and Data spaces.

;C ,    x --           append cell to dict
;   HERE ! 1 CELLS ALLOT ;
    head COMMA,2CH,docolon
        DW24 HERE,STORE,LIT,1,CELLS,ALLOT,EXIT

;C C,   char --        append char to dict
;   HERE C! 1 CHARS ALLOT ;
    head CCOMMA,{"C,"},docolon
        DW24 HERE,CSTORE,LIT,1,CHARS,ALLOT,EXIT

; INTERPRETER ===================================
; Note that NFA>LFA, NFA>CFA, IMMED?, and FIND
; are dependent on the structure of the Forth
; header.  This may be common across many CPUs,
; or it may be different.

;C SOURCE   -- adr n    current input buffer
;   'SOURCE 2@ ;        length is at lower adrs
    head SOURCE,{"SOURCE"},docolon
        DW24 TICKSOURCE,TWOFETCH,EXIT

;X /STRING  a u n -- a+n u-n   trim string
;   ROT OVER + ROT ROT - ;
    head SLASHSTRING,{"/STRING"},docolon
        DW24 ROT,OVER,PLUS,ROT,ROT,MINUS,EXIT

;Z >counted  src n dst --     copy to counted str
;   2DUP C! CHAR+ SWAP CMOVE ;
    head TOCOUNTED,{">COUNTED"},docolon
        DW24 TWODUP,CSTORE,CHARPLUS,SWOP,CMOVE,EXIT

;C WORD   char -- c-addr n   word delim'd by char
;   DUP  SOURCE >IN @ /STRING   -- c c adr n
;   DUP >R   ROT SKIP           -- c adr' n'
;   OVER >R  ROT SCAN           -- adr" n"
;   DUP IF CHAR- THEN        skip trailing delim.
;   R> R> ROT -   >IN +!        update >IN offset
;   TUCK -                      -- adr' N
;   HERE >counted               --
;   HERE                        -- a
;   BL OVER COUNT + C! ;    append trailing blank
    head WORD,{"WORD"},docolon
        DW24 DUP,SOURCE,TOIN,FETCH,SLASHSTRING
        DW24 DUP,TOR,ROT,SKIP
        DW24 OVER,TOR,ROT,SCAN
        DW24 DUP,QBRANCH,WORD1,ONEMINUS  ; char-
WORD1:  DW24 RFROM,RFROM,ROT,MINUS,TOIN,PLUSSTORE
        DW24 TUCK,MINUS
        DW24 HERE,TOCOUNTED,HERE
        ;DW24 PAD,TOCOUNTED,PAD
        DW24 BL,OVER,COUNT,PLUS,CSTORE,EXIT

;Z NFA>LFA   nfa -- lfa    name adr -> link field
;   3 - ;
    head NFATOLFA,{"NFA>LFA"},docolon
        DW24 LIT,4,MINUS,EXIT

;Z NFA>CFA   nfa -- cfa    name adr -> code field
;   COUNT 7F AND + ;       mask off 'smudge' bit
    head NFATOCFA,{"NFA>CFA"},docolon
        DW24 COUNT,LIT,07FH,AND,PLUS,EXIT

;Z IMMED?    nfa -- f      fetch immediate flag
;   1- C@ ;                     nonzero if immed
    head IMMEDQ,{"IMMED?"},docolon
        DW24 ONEMINUS,CFETCH,EXIT

;C FIND   c-addr -- c-addr 0   if not found
;C                  xt  1      if immediate
;C                  xt -1      if "normal"
;   LATEST @ BEGIN             -- a nfa
;       2DUP OVER C@ CHAR+     -- a nfa a nfa n+1
;       S=                     -- a nfa f
;       DUP IF
;           DROP
;           NFA>LFA @ DUP      -- a link link
;       THEN
;   0= UNTIL                   -- a nfa  OR  a 0
;   DUP IF
;       NIP DUP NFA>CFA        -- nfa xt
;       SWAP IMMED?            -- xt iflag
;       0= 1 OR                -- xt 1/-1
;   THEN ;
    head FIND,{"FIND"},docolon
        DW24 LATEST,FETCH
FIND1:  DW24 dbg,TWODUP,OVER,CFETCH,CHARPLUS
        DW24 SEQUAL,DUP,QBRANCH,FIND2
        DW24 DROP,NFATOLFA,FETCH,DUP
FIND2:  DW24 ZEROEQUAL,QBRANCH,FIND1
        DW24 DUP,QBRANCH,FIND3
        DW24 NIP,DUP,NFATOCFA
        DW24 SWOP,IMMEDQ,ZEROEQUAL,LIT,1,OR
FIND3:  DW24 EXIT

;C LITERAL  x --        append numeric literal
;   STATE @ IF ['] LIT ,XT , THEN ; IMMEDIATE
; This tests STATE so that it can also be used
; interpretively.  (ANSI doesn't require this.)
    immed LITERAL,{"LITERAL"},docolon
        DW24 STATE,FETCH,QBRANCH,LITER1
        DW24 LIT,LIT,COMMAXT,COMMA
LITER1: DW24 EXIT

;Z DIGIT?   c -- n -1   if c is a valid digit
;Z            -- x  0   otherwise
;   [ HEX ] DUP 39 > 100 AND +     silly looking
;   DUP 140 > 107 AND -   30 -     but it works!
;   DUP BASE @ U< ;
    head DIGITQ,{"DIGIT?"},docolon
        DW24 DUP,LIT,39H,GREATER,LIT,100H,AND,PLUS
        DW24 DUP,LIT,140H,GREATER,LIT,107H,AND
        DW24 MINUS,LIT,30H,MINUS
        DW24 DUP,BASE,FETCH,ULESS,EXIT

;Z ?SIGN   adr n -- adr' n' f  get optional sign
;Z  advance adr/n if sign; return NZ if negative
;   OVER C@                 -- adr n c
;   2C - DUP ABS 1 = AND    -- +=-1, -=+1, else 0
;   DUP IF 1+               -- +=0, -=+2
;       >R 1 /STRING R>     -- adr' n' f
;   THEN ;
    head QSIGN,{"?SIGN"},docolon
        DW24 OVER,CFETCH,LIT,2CH,MINUS,DUP,ABS
        DW24 LIT,1,EQUAL,AND,DUP,QBRANCH,QSIGN1
        DW24 ONEPLUS,TOR,LIT,1,SLASHSTRING,RFROM
QSIGN1: DW24 EXIT

;C >NUMBER  ud adr u -- ud' adr' u'
;C                      convert string to number
;   BEGIN
;   DUP WHILE
;       OVER C@ DIGIT?
;       0= IF DROP EXIT THEN
;       >R 2SWAP BASE @ UD*
;       R> M+ 2SWAP
;       1 /STRING
;   REPEAT ;
    head TONUMBER,{">NUMBER"},docolon
TONUM1: DW24 DUP,QBRANCH,TONUM3
        DW24 OVER,CFETCH,DIGITQ
        DW24 ZEROEQUAL,QBRANCH,TONUM2,DROP,EXIT
TONUM2: DW24 TOR,TWOSWAP,BASE,FETCH,UDSTAR
        DW24 RFROM,MPLUS,TWOSWAP
        DW24 LIT,1,SLASHSTRING,BRANCH,TONUM1
TONUM3: DW24 EXIT

;Z ?NUMBER  c-addr -- n -1      string->number
;Z                 -- c-addr 0  if convert error
;   DUP  0 0 ROT COUNT      -- ca ud adr n
;   ?SIGN >R  >NUMBER       -- ca ud adr' n'
;   IF   R> 2DROP 2DROP 0   -- ca 0   (error)
;   ELSE 2DROP NIP R>
;       IF NEGATE THEN  -1  -- n -1   (ok)
;   THEN ;
    head QNUMBER,{"?NUMBER"},docolon
        DW24 DUP,LIT,0,DUP,ROT,COUNT
        DW24 QSIGN,TOR,TONUMBER,QBRANCH,QNUM1
        DW24 RFROM,TWODROP,TWODROP,LIT,0
        DW24 BRANCH,QNUM3
QNUM1:  DW24 TWODROP,NIP,RFROM,QBRANCH,QNUM2,NEGATE
QNUM2:  DW24 LIT,-1
QNUM3:  DW24 EXIT

;Z INTERPRET    i*x c-addr u -- j*x
;Z                      interpret given buffer
; This is a common factor of EVALUATE and QUIT.
; ref. dpANS-6, 3.4 The Forth Text Interpreter
;   'SOURCE 2!  0 >IN !
;   BEGIN
;   BL WORD DUP C@ WHILE        -- textadr
;       FIND                    -- a 0/1/-1
;       ?DUP IF                 -- xt 1/-1
;           1+ STATE @ 0= OR    immed or interp?
;           IF EXECUTE ELSE ,XT THEN
;       ELSE                    -- textadr
;           ?NUMBER
;           IF POSTPONE LITERAL     converted ok
;           ELSE COUNT TYPE 3F EMIT CR ABORT  err
;           THEN
;       THEN
;   REPEAT DROP ;
    head INTERPRET,{"INTERPRET"},docolon
        DW24 TICKSOURCE,TWOSTORE,LIT,0,TOIN,STORE
INTER1: DW24 BL,WORD,DUP,CFETCH,QBRANCH,INTER9
        DW24 FIND,QDUP,QBRANCH,INTER4
        DW24 ONEPLUS,STATE,FETCH,ZEROEQUAL,OR
        DW24 QBRANCH,INTER2
        DW24 EXECUTE,BRANCH,INTER3
INTER2: DW24 COMMAXT
INTER3: DW24 BRANCH,INTER8
INTER4: DW24 QNUMBER,QBRANCH,INTER5
        DW24 LITERAL,BRANCH,INTER6
INTER5: DW24 COUNT,TYPE,LIT,3FH,EMIT,CR,ABORT
INTER6:
INTER8: DW24 BRANCH,INTER1
INTER9: DW24 DROP,EXIT

;C EVALUATE  i*x c-addr u -- j*x  interprt string
;   'SOURCE 2@ >R >R  >IN @ >R
;   INTERPRET
;   R> >IN !  R> R> 'SOURCE 2! ;
    head EVALUATE,{"EVALUATE"},docolon
        DW24 TICKSOURCE,TWOFETCH,TOR,TOR
        DW24 TOIN,FETCH,TOR,INTERPRET
        DW24 RFROM,TOIN,STORE,RFROM,RFROM
        DW24 TICKSOURCE,TWOSTORE,EXIT

;C QUIT     --    R: i*x --    interpret from kbd
;   L0 LP !  R0 RP!   0 STATE !
;   BEGIN
;       TIB DUP TIBSIZE ACCEPT  SPACE
;       INTERPRET
;       STATE @ 0= IF CR ." OK" THEN
;   AGAIN ;
    head QUIT,{"QUIT"},docolon
        DW24 INIT_UART
        DW24 L0,FETCH,LP,STORE
        DW24 R0,FETCH,RPSTORE,LIT,0,STATE,STORE
QUIT1:  DW24 TIB,DUP,TIBSIZE,ACCEPT,SPACE
        DW24 INTERPRET
        DW24 STATE,FETCH,ZEROEQUAL,QBRANCH,QUIT2
        DW24 CR,XSQUOTE
        DB 3,"OK "
        DW24 TYPE
QUIT2:  DW24 BRANCH,QUIT1

;C ABORT    i*x --   R: j*x --   clear stk & QUIT
;   S0 SP!  QUIT ;
    head ABORT,{"ABORT"},docolon
        DW24 S0,FETCH,SPSTORE,QUIT   ; QUIT never returns

;Z ?ABORT   f c-addr u --      abort & print msg
;   ROT IF TYPE ABORT THEN 2DROP ;
    head QABORT,{"?ABORT"},docolon
        DW24 ROT,QBRANCH,QABO1,TYPE,ABORT
QABO1:  DW24 TWODROP,EXIT

;C ABORT"  i*x 0  -- i*x   R: j*x -- j*x  x1=0
;C         i*x x1 --       R: j*x --      x1<>0
;   POSTPONE S" POSTPONE ?ABORT ; IMMEDIATE
    immed ABORTQUOTE,{"ABORT\""},docolon
        DW24 SQUOTE
        DW24 LIT,QABORT,COMMAXT
        DW24 EXIT

;C '    -- xt           find word in dictionary
;   BL WORD FIND
;   0= ABORT" ?" ;
    head TICK,{"\'"},docolon
        DW24 BL,WORD,FIND,ZEROEQUAL,XSQUOTE
        DB 1,"?"
        DW24 QABORT,EXIT

;C CHAR   -- char           parse ASCII character
;   BL WORD 1+ C@ ;
    head CHAR,{"CHAR"},docolon
        DW24 BL,WORD,ONEPLUS,CFETCH,EXIT

;C [CHAR]   --          compile character literal
;   CHAR  ['] LIT ,XT  , ; IMMEDIATE
    immed BRACCHAR,{"[CHAR]"},docolon
        DW24 CHAR
        DW24 LIT,LIT,COMMAXT
        DW24 COMMA,EXIT

;C (    --                     skip input until )
;   [ HEX ] 29 WORD DROP ; IMMEDIATE
    immed PAREN,{"("},docolon
        DW24 LIT,29H,WORD,DROP,EXIT

; COMPILER ======================================

;C CREATE   --      create an empty definition
;   LATEST @ , 0 C,         link & immed field
;   HERE LATEST !           new "latest" link
;   BL WORD C@ 1+ ALLOT         name field
;   docreate ,CF                code field
    head CREATE,{"CREATE"},docolon
        DW24 LATEST,FETCH,COMMA,LIT,0,CCOMMA
        DW24 HERE,LATEST,STORE
        DW24 BL,WORD,CFETCH,ONEPLUS,ALLOT
        DW24 LIT,docreate,COMMACF,EXIT

;Z (DOES>)  --      run-time action of DOES>
;   R>              adrs of headless DOES> def'n
;   LATEST @ NFA>CFA    code field to fix up
;   !CF ;
    head XDOES,{"(DOES>)"},docolon
        DW24 RFROM,LATEST,FETCH,NFATOCFA,STORECF
        DW24 EXIT

;C DOES>    --      change action of latest def'n
;   COMPILE (DOES>)
;   dodoes ,CF ; IMMEDIATE
    immed DOES,{"DOES>"},docolon
        DW24 LIT,XDOES,COMMAXT
        DW24 LIT,dodoes,COMMACF,EXIT

;C RECURSE  --      recurse current definition
;   LATEST @ NFA>CFA ,XT ; IMMEDIATE
    immed RECURSE,{"RECURSE"},docolon
        DW24 LATEST,FETCH,NFATOCFA,COMMAXT,EXIT

;C [        --      enter interpretive state
;   0 STATE ! ; IMMEDIATE
    immed LEFTBRACKET,{"["},docolon
        DW24 LIT,0,STATE,STORE,EXIT

;C ]        --      enter compiling state
;   -1 STATE ! ;
    head RIGHTBRACKET,{"]"},docolon
        DW24 LIT,-1,STATE,STORE,EXIT

;Z HIDE     --      "hide" latest definition
;   LATEST @ DUP C@ 80 OR SWAP C! ;
    head HIDE,{"HIDE"},docolon
        DW24 LATEST,FETCH,DUP,CFETCH,LIT,80H,OR
        DW24 SWOP,CSTORE,EXIT

;Z REVEAL   --      "reveal" latest definition
;   LATEST @ DUP C@ 7F AND SWAP C! ;
    head REVEAL,{"REVEAL"},docolon
        DW24 LATEST,FETCH,DUP,CFETCH,LIT,7FH,AND
        DW24 SWOP,CSTORE,EXIT

;C IMMEDIATE   --   make last def'n immediate
;   1 LATEST @ 1- C! ;   set immediate flag
    head IMMEDIATE,{"IMMEDIATE"},docolon
        DW24 LIT,1,LATEST,FETCH,ONEMINUS,CSTORE
        DW24 EXIT

;C :        --      begin a colon definition
;   CREATE HIDE ] !COLON ;
    head COLON,{":"},docode
        CALL docolon    ; code fwd ref explicitly
        DW24 CREATE,HIDE,RIGHTBRACKET,STORCOLON
        DW24 EXIT

;C ;
;   REVEAL  ,EXIT
;   POSTPONE [  ; IMMEDIATE
    immed SEMICOLON,{";"},docolon
        DW24 REVEAL,CEXIT
        DW24 LEFTBRACKET,EXIT

;C [']  --         find word & compile as literal
;   '  ['] LIT ,XT  , ; IMMEDIATE
; When encountered in a colon definition, the
; phrase  ['] xxx  will cause   LIT,xxt  to be
; compiled into the colon definition (where
; (where xxt is the execution token of word xxx).
; When the colon definition executes, xxt will
; be put on the stack.  (All xt's are one cell.)
    immed BRACTICK,{"[\']"},docolon
        DW24 TICK               ; get xt of 'xxx'
        DW24 LIT,LIT,COMMAXT    ; append LIT action
        DW24 COMMA,EXIT         ; append xt literal

;C POSTPONE  --   postpone compile action of word
;   BL WORD FIND
;   DUP 0= ABORT" ?"
;   0< IF   -- xt  non immed: add code to current
;                  def'n to compile xt later.
;       ['] LIT ,XT  ,      add "LIT,xt,COMMAXT"
;       ['] ,XT ,XT         to current definition
;   ELSE  ,XT      immed: compile into cur. def'n
;   THEN ; IMMEDIATE
    immed POSTPONE,{"POSTPONE"},docolon
        DW24 BL,WORD,FIND,DUP,ZEROEQUAL,XSQUOTE
        DB 1,"?"
        DW24 QABORT,ZEROLESS,QBRANCH,POST1
        DW24 LIT,LIT,COMMAXT,COMMA
        DW24 LIT,COMMAXT,COMMAXT,BRANCH,POST2
POST1:  DW24 COMMAXT
POST2:  DW24 EXIT

;Z COMPILE   --   append inline execution token
;   R> DUP CELL+ >R @ ,XT ;
; The phrase ['] xxx ,XT appears so often that
; this word was created to combine the actions
; of LIT and ,XT.  It takes an inline literal
; execution token and appends it to the dict.
;    head COMPILE,{"COMPILE"},docolon
;        DW24 RFROM,DUP,CELLPLUS,TOR
;        DW24 FETCH,COMMAXT,EXIT
; N.B.: not used in the current implementation

; CONTROL STRUCTURES ============================

;C IF       -- adrs    conditional forward branch
;   ['] qbranch ,BRANCH  HERE DUP ,DEST ;
;   IMMEDIATE
    immed IFF,{"IF"},docolon
        DW24 LIT,QBRANCH,COMMABRANCH
        DW24 HERE,DUP,COMMADEST,EXIT

;C THEN     adrs --        resolve forward branch
;   HERE SWAP !DEST ; IMMEDIATE
    immed THEN,{"THEN"},docolon
        DW24 HERE,SWOP,STOREDEST,EXIT

;C ELSE     adrs1 -- adrs2    branch for IF..ELSE
;   ['] branch ,BRANCH  HERE DUP ,DEST
;   SWAP  POSTPONE THEN ; IMMEDIATE
    immed ELLSE,{"ELSE"},docolon
        DW24 LIT,BRANCH,COMMABRANCH
        DW24 HERE,DUP,COMMADEST
        DW24 SWOP,THEN,EXIT

;C BEGIN    -- adrs        target for bwd. branch
;   HERE ; IMMEDIATE
    immed BEGIN,{"BEGIN"},docode
        jp HERE

;C UNTIL    adrs --   conditional backward branch
;   ['] qbranch ,BRANCH  ,DEST ; IMMEDIATE
;   conditional backward branch
    immed UNTIL,{"UNTIL"},docolon
        DW24 LIT,QBRANCH,COMMABRANCH
        DW24 COMMADEST,EXIT

;X AGAIN    adrs --      uncond'l backward branch
;   ['] branch ,BRANCH  ,DEST ; IMMEDIATE
;   unconditional backward branch
    immed AGAIN,{"AGAIN"},docolon
        DW24 LIT,BRANCH,COMMABRANCH
        DW24 COMMADEST,EXIT

;C WHILE    -- adrs         branch for WHILE loop
;   POSTPONE IF ; IMMEDIATE
    immed WHILE,{"WHILE"},docode
        jp IFF

;C REPEAT   adrs1 adrs2 --     resolve WHILE loop
;   SWAP POSTPONE AGAIN POSTPONE THEN ; IMMEDIATE
    immed REPEAT,{"REPEAT"},docolon
        DW24 SWOP,AGAIN,THEN,EXIT

;Z >L   x --   L: -- x        move to leave stack
;   CELL LP +!  LP @ ! ;      (L stack grows up)
    head TOL,{">L"},docolon
        DW24 CELL,LP,PLUSSTORE,LP,FETCH,STORE,EXIT

;Z L>   -- x   L: x --      move from leave stack
;   LP @ @  CELL NEGATE LP +! ;
    head LFROM,{"L>"},docolon
        DW24 LP,FETCH,FETCH
        DW24 CELL,NEGATE,LP,PLUSSTORE,EXIT

;C DO       -- adrs   L: -- 0
;   ['] xdo ,XT   HERE     target for bwd branch
;   0 >L ; IMMEDIATE           marker for LEAVEs
    immed DO,{"DO"},docolon
        DW24 LIT,XDO,COMMAXT,HERE
        DW24 LIT,0,TOL,EXIT

;Z ENDLOOP   adrs xt --   L: 0 a1 a2 .. aN --
;   ,BRANCH  ,DEST                backward loop
;   BEGIN L> ?DUP WHILE POSTPONE THEN REPEAT ;
;                                 resolve LEAVEs
; This is a common factor of LOOP and +LOOP.
    head ENDLOOP,{"ENDLOOP"},docolon
        DW24 COMMABRANCH,COMMADEST
LOOP1:  DW24 LFROM,QDUP,QBRANCH,LOOP2
        DW24 THEN,BRANCH,LOOP1
LOOP2:  DW24 EXIT

;C LOOP    adrs --   L: 0 a1 a2 .. aN --
;   ['] xloop ENDLOOP ;  IMMEDIATE
    immed LOOP,{"LOOP"},docolon
        DW24 LIT,XLOOP,ENDLOOP,EXIT

;C +LOOP   adrs --   L: 0 a1 a2 .. aN --
;   ['] xplusloop ENDLOOP ;  IMMEDIATE
    immed PLUSLOOP,{"+LOOP"},docolon
        DW24 LIT,XPLUSLOOP,ENDLOOP,EXIT

;C LEAVE    --    L: -- adrs
;   ['] UNLOOP ,XT
;   ['] branch ,BRANCH   HERE DUP ,DEST  >L
;   ; IMMEDIATE      unconditional forward branch
    immed LEAVE,{"LEAVE"},docolon
        DW24 LIT,UNLOOP,COMMAXT
        DW24 LIT,BRANCH,COMMABRANCH
        DW24 HERE,DUP,COMMADEST,TOL,EXIT

; OTHER OPERATIONS ==============================

;X WITHIN   n1|u1 n2|u2 n3|u3 -- f   n2<=n1<n3?
;  OVER - >R - R> U< ;          per ANS document
    head WITHIN,{"WITHIN"},docolon
        DW24 OVER,MINUS,TOR,MINUS,RFROM,ULESS,EXIT

;C MOVE    addr1 addr2 u --     smart move
;             VERSION FOR 1 ADDRESS UNIT = 1 CHAR
;  >R 2DUP SWAP DUP R@ +     -- ... dst src src+n
;  WITHIN IF  R> CMOVE>        src <= dst < src+n
;       ELSE  R> CMOVE  THEN ;          otherwise
    head MOVE,{"MOVE"},docolon
        DW24 TOR,TWODUP,SWOP,DUP,RFETCH,PLUS
        DW24 WITHIN,QBRANCH,MOVE1
        DW24 RFROM,CMOVEUP,BRANCH,MOVE2
MOVE1:  DW24 RFROM,CMOVE
MOVE2:  DW24 EXIT

;C DEPTH    -- +n        number of items on stack
;   SP@ S0 SWAP - 2/ ;   16-BIT VERSION!
    head DEPTH,{"DEPTH"},docolon
        DW24 SPFETCH,S0,SWOP,MINUS,TWOSLASH,EXIT

;C ENVIRONMENT?  c-addr u -- false   system query
;                         -- i*x true
;   2DROP 0 ;       the minimal definition!
    head ENVIRONMENTQ,{"ENVIRONMENT?"},docolon
        DW24 TWODROP,LIT,0,EXIT

; UTILITY WORDS AND STARTUP =====================

;X WORDS    --          list all words in dict.
;   LATEST @ BEGIN
;       DUP COUNT TYPE SPACE
;       NFA>LFA @
;   DUP 0= UNTIL
;   DROP ;
    head WORDS,{"WORDS"},docolon
        DW24 LATEST,FETCH
WDS1:   DW24 DUP,COUNT,TYPE,SPACE,NFATOLFA,FETCH
        DW24 DUP,ZEROEQUAL,QBRANCH,WDS1
        DW24 DROP,EXIT

;X .S      --           print stack contents
;   SP@ S0 - IF
;       SP@ S0 2 - DO I @ U. -2 +LOOP
;   THEN ;
    head DOTS,{".S"},docolon
        DW24 SPFETCH,S0,FETCH,MINUS,QBRANCH,DOTS2
        DW24 SPFETCH,S0,FETCH,CELL,MINUS,XDO
DOTS1:  DW24 II,FETCH,UDOT,CELL,NEGATE,XPLUSLOOP,DOTS1
DOTS2:  DW24 EXIT

;Z COLD     --      cold start Forth system
;   UINIT U0 #INIT CMOVE      init user area
;   80 COUNT INTERPRET       interpret CP/M cmd
;   ." eZ80F91 CamelForth etc."
;   ABORT ;
    head COLD,{"COLD"},docolon
        DW24 INIT_UART            ;initialize UART for terminal I/O
        DW24 UINIT,U0,NINIT,CMOVE     ;initialize User area
;		DW24 LIT,InpBuffer,COUNT,INTERPRET
;       DW24 LIT,0398h,TMR1_INIT      ;turn on 1-msec timer
        DW24 XSQUOTE      ; sign-on banner to console
        DB $F-$-1,0Dh,0Ah,0Dh,0Ah
        DB "eZ80F91 Camel FORTH "
        VERSION
        DB ", 1 APR 2021",0Dh,0Ah
$$:     DW24 TYPE,ABORT       ; ABORT never returns

;
;  D.R  dword nlen --       print dword in field nlen long
;
        head    DDOTR,{"D.R"},docolon
        DW24      TOR,SWOP,OVER,DABS,LESSNUM,NUMS,SIGN
        DW24      NUMGREATER,RFROM,OVER,MINUS,SPACES,TYPE
        DW24      EXIT

;
;  .R       n len --    print n in field len long
;
        head    DOTR,{".R"},docolon
        DW24      TOR,STOD,RFROM,DDOTR
        DW24      EXIT


; Karl Lundt's version of memory dump
;
;  DUMP naddr nknt -- display nknt bytes of memory from naddr
;
        head    DUMP,{"DUMP"},docolon
        DW24      HEX,CR,LIT,9,SPACES
        DW24      LIT,16,LIT,0,XDO
DUMP1   DW24      II,LIT,3,DOTR
        DW24      XLOOP,DUMP1
        DW24      LIT,2,SPACES,LIT,16,LIT,0,XDO
DUMP2   DW24      II,LIT,0,LESSNUM,NUM,NUMGREATER,TYPE
        DW24      XLOOP,DUMP2
        DW24      OVER,PLUS,SWOP,DUP,LIT,0fh
        DW24      AND,XOR,XDO
DUMP3   DW24      CR,II,LIT,0,LIT,8,DDOTR,LIT,1
        DW24      SPACES,II,LIT,16,PLUS,II
        DW24      OVER,OVER,XDO
DUMP4   DW24      II,CFETCH,SPACE,LIT,0,LESSNUM,NUM,NUM
        DW24      NUMGREATER,TYPE
        DW24      XLOOP,DUMP4
        DW24      LIT,2,SPACES,XDO
DUMP5   DW24      II,CFETCH,DUP,LIT,32,LESS
        DW24      QBRANCH,DUMP6
        DW24      DROP,LIT,46
DUMP6   DW24      DUP,LIT,126,GREATER
        DW24      QBRANCH,DUMP7
        DW24      DROP,LIT,46
DUMP7   DW24      EMIT
        DW24      XLOOP,DUMP5
        DW24      LIT,16
        DW24      XPLUSLOOP,DUMP3
        DW24      CR,EXIT

else
	ds 1	; makes the relist happy
endif ;INCLUDED
	END
