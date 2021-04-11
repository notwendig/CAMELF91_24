; CAMLF91e.S: Code Primitives
;   Source code is for the Zilog Macro Assembler.
;   Forth words are documented as follows:
;   NAME     instack -- outstack    description
;   where x=C for ANS Forth Core words, X for ANS
;   Extensions, Z for internal or private words.
;
; Direct-Threaded Forth model for Zilog Z80
; 16 bit cell, 8 bit char, 8 bit (byte) adrs unit
;        BC =       TOS  'Top Of Stack',holds top Param Stack item
;        HL =       W    working register
;        DE =       IP   Interpreter Pointer
;        SP =       PSP  Param Stack Pointer
;        IX =       RSP  Return Stack Pointer
;        IY =       UP   User area Pointer
;    A, alternate register set = temporaries
;
; Revision history:
;   19 Aug 94 v1.0
;   25 Jan 95 v1.01  now using BDOS function 0Ah
;       for interpreter input; TIB at 82h.
;   02 Mar 95 v1.02  changed ALIGN to ALIGNED in
;       S" (S"); changed ,BRANCH to ,XT in DO.
;   17 NOV 1998 SOURCE CODE CONVERTED TO ZMA v1.2
;       (Zilog Macro Assembler) by D. Beattie Jr.
;   27 FEB 2000 PORTED TO TRS-80 by D. Beattie Jr.
;   25 SEP 2001 PORTED TO eZ80 by D. Beattie Jr.
;   12 MAY 2003 v1.10e for eZ80 Acclaim, ZDS II (ZiLOG) XTools
;   23 JAN 2004 v1.00e for eZ80F91
;	01 APR 2021 v1.00e-24bit for eZ80F91 JSievers@NadiSoft.de

    .INCLUDE "eZ80F91.INC"    ; CPU Equates
	.INCLUDE "intvect.inc"
	.INCLUDE "CAMELF91.INC"

	DEFINE CAMELF91E,SPACE=rom
	SEGMENT CAMELF91E
	.ASSUME ADL=1
	
; Memory map for eZ80F91 Evaluation Platform:
;   0000h       Forth kernel in ROM
;   0E000h      Terminal Input Buffer, 128 bytes (eZ80F91 on-chip RAM)
;   0E080h      Forth dictionary (eZ80F91 on-chip RAM)
;   EM-200h     User area, 128 bytes
;   EM-180h     Parameter stack, 128 bytes, grows down
;   EM-100h     HOLD area, 40 bytes, grows down
;   EM-0D8h     PAD buffer, 88 bytes
;   EM-80h      Return stack, 128 bytes, grows down
;   0FFFDh      End of Memory ('EM') (End of RAM)
; See also the definitions of U0, S0, and R0
; in the "system variables & constants" area.
; A task w/o terminal input requires 200h bytes.
; Double all except TIB and PAD for 32-bit CPUs.

; RESET AND INTERRUPT VECTORS ===================

    ;ORG 0000h
    ;JP  ENTRY
;    ORG 08h        ;RST 1 vector
;    RET
;    DW24 RST08ISR
;    ORG 10h        ;RST 2 vector
;    RET
;    ORG 18h        ;RST 3 vector
;    RET
;           ...etc.  (for later implementation)
;

    ;ORG     ENTRY   ; program ENTRY POINT
	; void * _set_vector(unsigned int vector, void(*handler)(void));
	xref _set_vector
	
	xdef ENTRY
ENTRY: ;reset:
	DI
	IN0		a,(PD_ALT2)
	OR		a,ffh
	OUT0	(PD_ALT2),a
	IN0		a,(PD_ALT1)
	OR		a,0
	OUT0	(PD_ALT1),a
	IN0		a,(PD_DDR)
	OR		a,ffh
	OUT0	(PD_DDR),a
	ld		a,0
	out0	(UART0_IER), a
	ld		hl,Uart0IRQ
	push	hl
	ld		hl,UART0_IVECT
	push	hl
	call	_set_vector
	call	uart0

; ********************************************************
; ** ( initialize registers, and jump to COLD )
	ld	ix,(def_R0)	; = top of return stack
	ld	sp,(def_S0)	; = top of param stack
	ld	iy,(def_U0)	; = bottom of user area
	
    ld de,1      ; do reset if COLD returns
    LD  A,0Dh
    LD  (InpBuffer),A
	EI
    jp COLD      ; enter top-level Forth word2


;Z debug break  --    
    head dbg,{"DBG"},docode
	nop
	next

;Z INIT-UART  --    Initialize UART and activate driver/receiver
; note that both COLD and QUIT call this procedure.
;   user program can also call it if needed.
    head INIT_UART,{"INIT-UART"},docode
	call	uart0
	next

Uart0IRQ:	push	af
			in0		a, (UART0_IER)		; read the UART0 interrupt enable
			and		a, 0FEh				; receive enable irq is bit 0
			out0	(UART0_IER), a		; clear the receive interrupt
			in0		a, (UART0_SPR)		; read the HW flow cntrl setup
			;see if we are using hardware flow control
			bit		6, a				; are we using flow control
			jr		z, $F				; if b6 = 0 no flow control
			;Implement flow control. b2,b3 have to be in b0, b1
			;to be copied to the modem line control register
			rra
			rra
			and		a, 03h				; mask the receive disabled bits
			push	bc
			ld		b, a				; save the bits we need
			in0		a, (UART0_MCTL)		; Get RTS, DTR inactivate.
			and		a, 0FCh				; force lines lows
			or		a, b				; set desired lines high
			out0	(UART0_MCTL), a		; Set RTS, DTR to inactivate state.
			pop		bc
$$:			pop		af
			ei
			reti
			
uart0:	
	; Configure UART0 for 115200,8,1,n. Tx flow DSR, Rx flow DTR, RTS
			ld		a, C7h
			out0	(UART0_FCTL), a		; Enable FIFO and clear, int after 14 bytes.
			ld		a, 80h
			out0	(UART0_LCTL), a		; Enable access to BRG.
			ld		a, LOW(DIVISOR)
			out0	(UART0_BRG_L), a	; Load low byte of BRG.
			ld		a, HIGH(DIVISOR)
			out0	(UART0_BRG_H), a	; Load high byte of BRG.
			ld		a, 03h
			out0	(UART0_LCTL), a		; Select 8 bits, no parity, 1 stop.
			ld		a, 03h
			out0	(UART0_MCTL), a		; Select activate RTS, DTR
			IN0     A,(UART0_FCTL)
			OR		A,06h
			OUT0    (UART0_FCTL),A		; Enable UART Rx/Tx
			
			ld		a,11010011b			; D3h. b6 = rx flow, 
										; set RTS=0 and DTR=0 for no rx (b3, b2)
										; set RTS=1 and DTR=1 for rx (b1, b0)
										; b7 = tx flow,
										; DSR = 0, CTS = 1 for tx allowed (b5, b4)
			out0	(UART0_SPR), a		; save the settings in the scratch register		
										; setup for a receive IRQ
			ld		a, 01h				; receive enable irq is bit 1
			out0	(UART0_IER), a		; enable the receive interrupt
			ret

;
; check if character ready to receive
;
RCXRDY:
if 0
        IN0     A,(UART0_LSR)   ; check for char waiting..
        AND     UART_DR
        RET
else
			in0		a, (UART0_LSR)		; Read UART status register of COM Port 0.
			bit		0, a				; Test character ready bit.
			jr		z, uart0st1			; If not Zero then character available.
			or		a, 0FFh				; Set character is available flag.
			ret							; Done.
			; no data so enable hardware flow control
			; receive irq will disable RTS, DTR when fifo full
uart0st1:	in0		a, (UART0_SPR)	; read HW flow register
			bit		6, a			; see if we are using flow control
			jr		z, uart0st3		; if no flow control return status
			; we are using receive flow control
			in0		a, (UART0_IER)	; read irq enable register
			bit		0, a			; Test irq already enabled.
			jr		nz, uart0st3	; If Z=0 then IRQ enabled.
			; enable flow control
			in0		a, (UART0_SPR)	; read flow control settings
			and		a, 03h			; mask off rx enabled bits
			push	bc
			ld      b, a			; save the rx flow bits
			in0		a, (UART0_MCTL)	; get the modem line settings
			and		a, 0FCh			; set all rx flow bits low
			or		a, b			; set required lines high
			out0	(UART0_MCTL),a	; Set RTS, DTR to rx enabled state
			pop		bc
			; setup for a receive IRQ
			ld		a, 01h			; receive enable irq is bit 1
			out0	(UART0_IER), a	; enable the receive interrupt
uart0st3:	xor		a				; Set character not available flag.
			ret						; Done.
			
TMXRDY:		in0		a, (UART0_SPR)	; read flow control bits
			bit		7, a			; see if we are using flow control
			jr		nz, uart0ost1	; if zero no tx flow control
			; we are not using tx flow control, ok to use tx fifo
			in0		a,(UART0_LSR)	; Read UART status register of COM port 1.
			bit		5, a			; Test TX Data/shift register Empty ready bit
			jr		z, uart0ostX	; if bit zero we can not transmit
			or		a, 0FFh			; indicate we can transmit
			ret						; return to caller
			; using tx flow control, do not use tx fifo
uart0ost1:	in0		a, (UART0_LSR)	; Read UART status register of COM port 1.
			bit		6, a			; Test TX Data Register Empty ready bit
uart0ost2:	jr		z, uart0ostX	; if zero nothing can be sent
			; we are allowed to send if flow control lines allow it
			push	bc
			in0		a, (UART0_MSR)	; read current line settings
			and		a, 30h			; mask off the DSR, CTS (B5, B4)
			ld		b, a			; save the bits we need to check
			in0		a, (UART0_SPR)	; get required settings from flow cntrl reg
			and		a, 30h			; mask off the required tx flow bits
			ld		c, a			; save the required bits
			and		a, b			; logical and existing bits with required bits
			xor		a, c			; compare with required bits
			pop		bc
			jr		nz, uart0ostX	; if not the same can not tx
			or		a, 0FFh			; indicate we can transmit
			ret						; return to caller
			; seams like we can send
uart0ostX:  xor 	a, a
			ret			
endif
		
;
; transmit byte in reg C
;
	
TXMCHR:
	CALL	TMXRDY
    JR      Z,TXMCHR
    LD      A,C
    OUT0    (UART0_THR),A
    RET
;
; receive byte into reg A
;
RCXCHR:
	CALL	RCXRDY
    JR      Z,RCXCHR
    IN0     A,(UART0_RBR)
    RET

; INTERPRETER LOGIC =============================
; See also "defining words" at end of this file

;C EXIT     --      exit a colon definition
    head EXIT,{"EXIT"},docode
		ld	de,(ix)    ; pop old IP from ret stk
        lea ix,ix+3
        next

;Z lit      -- x    fetch inline literal to stack
; This is the primitive compiled by LITERAL.
    head LIT,{"lit"},docode
        push bc 	; push old TOS
		ex	de,hl
        ld bc,(hl)     ; fetch cell at IP to TOS, advancing IP
		ex 	de,hl
        inc de
        inc de
		inc de
        next

;C EXECUTE   i*x xt -- j*x   execute Forth word
;C                           at 'xt'
    head EXECUTE,{"EXECUTE"},docode
        ld hl,bc          ; address of word -> HL
        pop bc          ; get new TOS
        jp (hl)         ; go do Forth word

; DEFINING WORDS ================================

; ENTER, a.k.a. DOCOLON, entered by CALL ENTER
; to enter a new high-level thread (colon def'n.)
; (internal code fragment, not a Forth word)
; N.B.: DOCOLON must be defined before any
; appearance of 'docolon' in a 'word' macro!

docolon:               					; (alternate name)
enter:  lea		ix,ix-3
        ld 		(ix),de   				; push old IP on ret stack
        pop 	hl         				; param field adrs -> IP
        nexthl         					; use the faster 'nexthl'

;C VARIABLE   --      define a Forth variable
;   CREATE 1 CELLS ALLOT ;
; Action of RAM variable is identical to CREATE,
; so we don't need a DOES> clause to change it.
    head VARIABLE,{"VARIABLE"},docolon
        DW24 CREATE,LIT,1,CELLS,ALLOT,EXIT
; DOVAR, code action of VARIABLE, entered by CALL
; DOCREATE, code action of newly created words

docreate:
dovar:  ; -- a-addr
        pop 	hl     					; parameter field address
        push 	bc    					; push old TOS
        ld 		bc,hl   				; pfa = variable's adrs -> TOS
        next

;C CONSTANT   n --      define a Forth constant
;   CREATE , DOES> (machine code fragment)
    head CONSTANT,{"CONSTANT"},docolon
        DW24 CREATE,COMMA,XDOES

; DOCON, code action of CONSTANT,
; entered by CALL DOCON
	xdef docon
docon:  ; -- x
        pop 	hl     					; parameter field address
        push 	bc    					; push old TOS
        ld 		bc,(hl) 				; fetch contents of parameter field -> TOS
        next

;Z USER     n --        define user variable "n"
;   CREATE , DOES> (machine code fragment)
    head USER,{"USER"},docolon
        DW24 CREATE,COMMA,XDOES

; DOUSER, code action of USER,
; entered by CALL DOUSER
	xdef douser
douser:  ; -- a-addr
        pop 	hl     					; parameter field address
        push 	bc    					; push old TOS
        ld 		bc,(hl)  				; fetch contents of parameter field
        ld 		hl,iy    				; copy user base address to HL
        add 	hl,bc  					;    and add offset
        ld 		bc,hl     				; put result in TOS
        next

; DODOES, code action of DOES> clause
; entered by       CALL fragment
;                  parameter field
;                       ...
;        fragment: CALL DODOES
;                  high-level thread
; Enters high-level thread with address of
; parameter field on top of stack.
; (internal code fragment, not a Forth word)
	xdef dodoes
dodoes: ; -- a-addr 
        lea		ix,ix-3
        ld 		(ix),de					; push old IP on ret stk
		pop 	de         				; adrs of new thread -> IP
        pop 	hl         				; adrs of parameter field
        push 	bc        				; push old TOS onto stack
        ld 		bc,hl         			; pfa -> new TOS
        next


;C EMIT     c --    output character to console

    head EMIT,{"EMIT"},docode
        CALL    TXMCHR
        POP     BC
        next

;C KEY?     -- f    return true if char waiting
    head QUERYKEY,{"KEY?"},docode
        PUSH 	BC     					; PREPARE FOR NEW TOS
        CALL 	RCXRDY
		ld		bc,0
        LD 		C,A      				; RESULT IN TOS
        next

;C KEY      -- c    get character from keyboard
    head KEY,{"KEY"},docode
        PUSH 	BC     					;PREPARE FOR NEW TOS
        CALL 	RCXCHR
		ld		bc,0
        LD 		C,A      				; RESULT IN TOS
        next


;X BYE     i*x --    return to CP/M
    head BYE,{"bye"},docode
        HALT
        jp 		0

; STACK OPERATIONS ==============================

;C DUP      x -- x x      duplicate top of stack
    head DUP,{"DUP"},docode
pushtos:push 	bc
     	next

;C ?DUP     x -- 0 | x x    DUP if nonzero
    head QDUP,{"?DUP"},docode
		or		a
		sbc		hl,hl
		sbc		hl,bc
        jr 		nz,pushtos
        next

;C DROP     x --          drop top of stack
    head DROP,{"DROP"},docode
poptos: pop 	bc
        next

;C SWAP     x1 x2 -- x2 x1    swap top two items
    head SWOP,{"SWAP"},docode
        pop 	hl
        push 	bc
        ld 		bc,hl
        next

;C OVER    x1 x2 -- x1 x2 x1   per stack diagram
    head OVER,{"OVER"},docode
        pop 	hl
        push 	hl
        push 	bc
        ld 		bc,hl
        next

;C ROT    x1 x2 x3 -- x2 x3 x1  per stack diagram
    head ROT,{"ROT"},docode
        ; x3 is in TOS
        pop 	hl          			; x2
        ex 		(sp),hl      			; x2 on stack, x1 in hl
        push 	bc
        ld 		bc,hl
        next

;X NIP    x1 x2 -- x2           per stack diagram
    head NIP,{"NIP"},docolon
        DW24 SWOP,DROP,EXIT

;X TUCK   x1 x2 -- x2 x1 x2     per stack diagram
    head TUCK,{"TUCK"},docolon
        DW24 SWOP,OVER,EXIT

;C >R    x --   R: -- x   push to return stack
    head TOR,{">R"},docode
        lea 	ix,ix-3
        ld 		(ix),bc    				; push TOS onto rtn stk
        pop 	bc          			; pop new TOS
        next

;C R>    -- x    R: x --   pop from return stack
    head RFROM,{"R>"},docode
        push 	bc         				; push old TOS
        ld 		bc,(ix)     			; pop top rtn stk item
        lea 	ix,ix+3    				;  to TOS
        next

;C R@    -- x     R: x -- x   fetch from rtn stk
    head RFETCH,{"R@"},docode
        push 	bc         				; push old TOS
        ld 		bc,(ix)     			; fetch top rtn stk item to TOS
        next

;Z SP@  -- a-addr       get data stack pointer
    head SPFETCH,{"SP@"},docode
        push 	bc
		or		a
        sbc		hl,hl
        add 	hl,sp
        ld 		bc,hl
        next

;Z SP!  a-addr --       set data stack pointer
    head SPSTORE,{"SP!"},docode
        ld 		hl,bc
        ld 		sp,hl
        pop 	bc          			; get new TOS
        next

;Z RP@  -- a-addr       get return stack pointer
    head RPFETCH,{"RP@"},docode
        push 	bc
        push 	ix
        pop 	bc
        next

;Z RP!  a-addr --       set return stack pointer
    head RPSTORE,{"RP!"},docode
        push 	bc
        pop 	ix
        pop 	bc
        next

; MEMORY AND I/O OPERATIONS =====================

;C !        x a-addr --   store cell in memory
    head STORE,{"!"},docode
        ld 		hl,bc          		; address in hl
        pop 	bc         			; data in bc
        ld		(hl),bc
        pop 	bc          		; pop new TOS
        next

;C C!      char c-addr --    store char in memory
    head CSTORE,{"C!"},docode
        ld 		hl,bc          		; address in hl
        pop 	bc          		; data in bc
        ld 		(hl),c
        pop 	bc          		; pop new TOS
        next

;C @       a-addr -- x   fetch cell from memory
    head FETCH,{"@"},docode
        ld 		hl,bc          		; address in hl
        ld 		bc,(hl)
        next

;C C@     c-addr -- char   fetch char from memory
    head CFETCH,{"C@"},docode
        ld 		a,(bc)
		ld		bc,0
        ld 		c,a
        next

; ---------- PORT I/O Commands

;C  P!  ( n p -- ) write byte n to i/o port p
    head PSTORE,{"P!"},docode
    	jr		PCSTORE

;C P@   ( p -- n )  read byte n from i/o port p
    head PFETCH,{"P@"},docode
    	jr		PCFETCH

;Z PC!     char c-addr --    output char to port
    head PCSTORE,{"PC!"},docode
       	pop 	hl          			; char in L
        out 	(bc),l       			; to port (BC)
        pop 	bc          			; pop new TOS
        next
	
;Z PC@     c-addr -- char   input char from port
    head PCFETCH,{"PC@"},docode
        in 		a,(bc)        			; read port (BC) to C
		ld 		bc,0
        ld 		c,a
        next

; ---------- eZ80 internal pherfery I/O Commands

;Z P0!    char c-addr --    output byte to port0
    head P0STORE,{"P0!"},docode
		ld		hl,C90009h
		ld  	h,c
		pop		bc						; 09 c-addr, C9
		push 	hl
		ld		hl,ED0000h
		push 	hl						; 00 00 ED
		or		a
		sbc		hl,hl
		add		hl,sp
		inc		hl
		inc		hl						; HL=> ED 09 c-addr C9 / OUT0 (c-addr),C RET
		call 	$F
		pop		hl
		pop		hl
		pop		bc
		next
$$:		jp		(hl)
		
;Z P0C@    c-addr -- char   input byte from port0
    head P0FETCH,{"P0@"},docode
		ld		hl,C90008h
		ld  	h,c
		push 	hl
		ld		hl,ED0000h
		push 	hl						; 00 00 ED
		or		a
		sbc		hl,hl
		add		hl,sp
		inc		hl
		inc		hl						; HL=> ED 08 c-addr C9 / IN0 C,(c-addr) RET
		call 	$F
		pop		hl
		pop		hl
		next
$$:		jp		(hl)

; ARITHMETIC AND LOGICAL OPERATIONS =============

;C +       n1/u1 n2/u2 -- n3/u3     add n1+n2
    head PLUS,{"+"},docode
        pop 	hl
        add 	hl,bc
        ld 		bc,hl
        next

;X M+       d n -- d         add single to double
    head MPLUS,{"M+"},docode
        ex 		de,hl
        pop 	de          			; hi cell
        ex 		(sp),hl      			; lo cell, save IP
        add 	hl,bc
        ld 		bc,de        			; hi result in BC (TOS)
        jr 		nc,mplus1
        inc 	bc
mplus1: pop 	de          			; restore saved IP
        push 	hl        	 			; push lo result
        next

;C -      n1/u1 n2/u2 -- n3/u3    subtract n1-n2
    head MINUS,{"-"},docode
        pop 	hl
        or 		a
        sbc 	hl,bc
        ld 		bc,hl
        next

;C AND    x1 x2 -- x3            logical AND
    head AND,{"AND"},docode
		push 	bc
		push 	ix
		ld		ix,0
		add		ix,sp
		ld		a,(ix+3)
		and		a,(ix+6)
		ld		(ix+6),a
		ld		a,(ix+4)
		and		a,(ix+7)
		ld		(ix+7),a
		ld		a,(ix+5)
		and		a,(ix+8)
		ld		(ix+8),a
		pop		ix
		pop		bc
		pop 	bc
		next

;C OR     x1 x2 -- x3           logical OR
    head OR,{"OR"},docode
        push 	bc
		push 	ix
		ld		ix,0
		add		ix,sp
		ld		a,(ix+3)
		or		a,(ix+6)
		ld		(ix+6),a
		ld		a,(ix+4)
		or		a,(ix+7)
		ld		(ix+7),a
		ld		a,(ix+5)
		or		a,(ix+8)
		ld		(ix+8),a
		pop		ix
		pop		bc
		pop 	bc
		next

;C XOR    x1 x2 -- x3            logical XOR
    head XOR,{"XOR"},docode
        push 	bc
		push 	ix
		ld		ix,0
		add	ix,sp
		ld	a,(ix+3)
		xor	a,(ix+6)
		ld	(ix+6),a
		ld	a,(ix+4)
		xor	a,(ix+7)
		ld	(ix+7),a
		ld	a,(ix+5)
		xor	a,(ix+8)
		ld	(ix+8),a
		pop	ix
		pop	bc
		pop bc
		next

;C INVERT   x1 -- x2            bitwise inversion
    head INVERT,{"INVERT"},docode
        push bc
		push ix
		ld	ix,0
		add	ix,sp
		ld	a,(ix+3)
		cpl
		ld	(ix+3),a
		ld	a,(ix+4)
		cpl
		ld	(ix+4),a
		ld	a,(ix+5)
		cpl
		ld	(ix+5),a
		pop	ix
		pop	bc
		next

;C NEGATE   x1 -- x2            two's complement
    head NEGATE,{"NEGATE"},docolon
       DW24	INVERT,ONEPLUS,EXIT

;C 1+      n1/u1 -- n2/u2       add 1 to TOS
    head ONEPLUS,{"1+"},docode
        inc bc
        next

;C 1-      n1/u1 -- n2/u2     subtract 1 from TOS
    head ONEMINUS,{"1-"},docode
        dec bc
        next

;Z ><      x1 -- x2         swap bytes (not ANSI)
    head SWAPBYTES,{"><"},docode
        ld a,b
        ld b,c
        ld c,a
        next

;C 2*      x1 -- x2         arithmetic left shift
    head TWOSTAR,{"2*"},docode
		push bc
		push ix
		ld	ix,0
		add	ix,sp
		sla (ix+3)
		rl	(ix+4)
		rl	(ix+5)
		pop	ix
		pop	bc
		next

;C 2/      x1 -- x2        arithmetic right shift
    head TWOSLASH,{"2/"},docode
		push bc
		push ix
		ld	ix,0
		add	ix,sp
		sra (ix+5)
		rr	(ix+4)
		rr	(ix+3)
		pop	ix
		pop	bc
        next

;C LSHIFT  x1 u -- x2    logical L shift u places
    head LSHIFT,{"LSHIFT"},docode
        ld b,c        ; b = loop counter
        pop hl        ;   NB: hi 8 bits ignored!
        inc b         ; test for counter=0 case
        jr lsh2
lsh1:   add hl,hl     ; left shift HL, n times
lsh2:   djnz lsh1
        ld bc,hl        ; result is new TOS
        next

;C RSHIFT  x1 u -- x2    logical R shift u places
    head RSHIFT,{"RSHIFT"},docode
        ld b,c        ; b = loop counter
		push ix
		ld	ix,0
		add	ix,sp
		inc b         ; test for counter=0 case
        jr rsh2
rsh1:	srl (ix+5)
		rr	(ix+4)
		rr	(ix+3)
rsh2:   djnz rsh1
		pop	ix
		pop bc
        next

;C +!     n/u a-addr --       add cell to memory
    head PLUSSTORE,{"+!"},docode
		push bc
		pop hl		; a-addr
		pop	bc		; n/u
		push de
        ld de,(hl)	; (mem)
		ex de,hl
        add hl,bc
		ex de,hl
		ld (hl),de
		pop	de
        pop bc          ; pop new TOS
        next

; COMPARISON OPERATIONS =========================

;C 0=     n/u -- flag    return true if TOS=0
    head ZEROEQUAL,{"0="},docode
		or	a
		sbc	hl,hl
		sbc	hl,bc
		ld	bc,0
		jr	nz,$F
		dec	bc
$$:     next

;C 0<     n -- flag      true if TOS negative
    head ZEROLESS,{"0<"},docode
		ld	hl,7FFFFFh
		or	a
		sbc	hl,bc
		ld	bc,0
		jr	nc,$F
		dec bc
$$:     next

;C =      x1 x2 -- flag         test x1=x2
    head EQUAL,{"="},docode
        pop hl
        or a
        sbc hl,bc       ; x1-x2 in HL, SZVC valid
		ld	bc,0
		jr nz,$F
		dec bc
$$:		next

;X <>     x1 x2 -- flag    test not eq (not ANSI)
    head NOTEQUAL,{"<>"},docolon
        DW24 EQUAL,ZEROEQUAL,EXIT

;C <      n1 n2 -- flag        test n1<n2, signed
    head LESS,{"<"},docode
        pop hl
        or a
        sbc hl,bc       ; n1-n2 in HL, SZVC valid
; if result negative & not OV, n1<n2
; neg. & OV => n1 +ve, n2 -ve, rslt -ve, so n1>n2
; if result positive & not OV, n1>=n2
; pos. & OV => n1 -ve, n2 +ve, rslt +ve, so n1<n2
; thus OV reverses the sense of the sign bit
		ld bc,0
        jp pe,revsense 
        jp p,$F  
		dec bc
$$      next
revsense: jp m,$B
        dec	bc
		next

;C >     n1 n2 -- flag         test n1>n2, signed
    head GREATER,{">"},docolon
        DW24 SWOP,LESS,EXIT

;C U<    u1 u2 -- flag       test u1<n2, unsigned
    head ULESS,{"U<"},docode
        pop hl
        or a
        sbc hl,bc       ; u1-u2 in HL, SZVC valid
		ld	bc,0
		jr  nc,$F
		dec	bc
$$:     next

;X U>    u1 u2 -- flag     u1>u2 unsgd (not ANSI)
    head UGREATER,{"U>"},docolon
        DW24 SWOP,ULESS,EXIT

; LOOP AND BRANCH OPERATIONS ====================

;Z branch   --                  branch always
    head BRANCH,{"branch"},docode
dobranch:ex de,hl
		ld	de,(hl)
		ex	de,hl
        nexthl

;Z ?branch   x --              branch if TOS zero
    head QBRANCH,{"?branch"},docode
		or	a
		ld	hl,0
		sbc	hl,bc
        pop bc          ; pop new TOS
        jr z,dobranch   ; if old TOS=0, branch
        inc de          ; else skip inline value
        inc de
		inc de
        next

;Z (do)    n1|u1 n2|u2 --  R: -- sys1 sys2
;Z                          run-time code for DO
; '83 and ANSI standard loops terminate when the
; boundary of limit-1 and limit is crossed, in
; either direction.  This can be conveniently
; implemented by making the limit 800000h, so that
; arithmetic overflow logic can detect crossing.
; I learned this trick from Laxen & Perry F83.
; fudge factor = 800000h-limit, to be added to
; the start value.
    head XDO,{"(do)"},docode
        ex de,hl
        ex (sp),hl   ; IP on stack, limit in HL
        ex de,hl
        ld hl,800000h
        or a
        sbc hl,de    ; 800000-limit in HL
        ld (ix-3),hl  ;push this fudge factor onto return stack
		add hl,bc    ; add fudge to start value
        ld (ix-6),hl  ;  push adjusted start value onto return stack
		lea	ix,ix-6
        pop de       ; restore the saved IP
        pop bc       ; pop new TOS
        next

;Z (loop)   R: sys1 sys2 --  | sys1 sys2
;Z                        run-time code for LOOP
; Add 1 to the loop index.  If loop terminates,
; clean up the return stack and skip the branch.
; Else take the inline branch.  Note that LOOP
; terminates when index=8000h.
    head XLOOP,{"(loop)"},docode
        exx
        ld bc,1
looptst:ld hl,(ix)  ; get the loop index
        or a
        adc hl,bc    ; increment w/overflow test
        jp pe,loopterm  ; overflow=loop done
        ; continue the loop
        ld (ix),hl  ; save the updated index
        exx
        jr dobranch  ; take the inline branch
loopterm: ; terminate the loop
        lea ix,ix+6  ; discard the loop info
        exx
        inc de       ; skip the inline branch
        inc de
		inc	de
        next

;Z (+loop)   n --   R: sys1 sys2 --  | sys1 sys2
;Z                        run-time code for +LOOP
; Add n to the loop index.  If loop terminates,
; clean up the return stack and skip the branch.
; Else take the inline branch.
    head XPLUSLOOP,{"(+loop)"},docode
        pop hl      ; this will be the new TOS
        push bc
        ld bc,hl
        exx
        pop bc      ; old TOS = loop increment
        jr looptst

;C I        -- n   R: sys1 sys2 -- sys1 sys2
;C                  get the innermost loop index
    head II,{"I"},docode
        push bc     ; push old TOS
        ld hl,(ix+0) ; get current loop index
        ld bc,(ix+3) ; get fudge factor
        or a
        sbc hl,bc   ; subtract fudge factor,
        ld bc,hl    ;   returning true index
        next

;C J        -- n   R: 4*sys -- 4*sys
;C                  get the second loop index
    head JJ,{"J"},docode
        push bc     ; push old TOS
        ld hl,(ix+6) ; get current loop index
        ld bc,(ix+9) ; get fudge factor
        or a
        sbc hl,bc   ; subtract fudge factor,
        ld bc,hl    ;   returning true index
        next

;C UNLOOP   --   R: sys1 sys2 --  drop loop parms
    head UNLOOP,{"UNLOOP"},docode
        lea ix,ix+6
        next

; MULTIPLY AND DIVIDE ===========================

;C UM*     u1 u2 -- ud   unsigned 16x16->32 mult.
    head UMSTAR,{"UM*"},docode
        ld hl,0     ; result will be in HLDE
		push hl	
		push ix
		ld	ix,0
		add	ix,sp	; sp ix,[3,4,5]hl,[6,7,8]de=u1
        ld a,25     ; loop counter
        or a        ; clear cy
umloop: rr (ix+5)
        rr (ix+4)
        rr (ix+3)	; hl,de >> 1
        rr (ix+8)
        rr (ix+7)
        rr (ix+6)
        jr nc,noadd
		ld	hl,(ix+3)
		add hl,bc
		ld	(ix+3),hl
noadd:  dec a
        jr nz,umloop
		pop ix
		pop bc      ; put hi TOS back in BC
        next

;C UM/MOD   ud u1 -- u2 u3   unsigned 32/16->16
    head UMSLASHMOD,{"UM/MOD"},docode
        push bc
        exx
        pop bc      ; BC = divisor
        pop hl      ; HLDE = dividend
        ;pop de
		push ix
		ld	ix,0
		add ix,sp	;ix, de
        ld a,24     ; loop counter
		sla (ix+3)
        rl	(ix+4)
        rl 	(ix+5)  ; hi bit DE -> carry
udloop: adc hl,hl   ; rot left w/ carry
        jr nc,udiv3
        ; case 1: 25 bit, cy:HL = 1xxxx
        or a        ; we know we can subtract
        sbc hl,bc
        or a        ; clear cy to indicate sub ok
        jr udiv4
        ; case 2: 16 bit, cy:HL = 0xxxx
udiv3:  sbc hl,bc   ; try the subtract
        jr nc,udiv4 ; if no cy, subtract ok
        add hl,bc   ; else cancel the subtract
        scf         ;   and set cy to indicate
udiv4:  rl (ix+3)   ; rotate result bit into DE,
        rl (ix+4)        
		rl (ix+5)	; and next bit of DE into cy
        dec a
        jr nz,udloop
        ; now have complemented quotient in DE,
        ; and remainder in HL
		ld a,(ix+3)
		cpl	
		ld (ix+3),a
		ld a,(ix+4)
		cpl
		ld (ix+4),a
		ld a,(ix+5)
		cpl
		ld (ix+5),a
		pop	ix
		pop	bc
        push hl     ; push remainder
        push bc
        exx
        pop bc      ; quotient remains in TOS
        next

; BLOCK AND STRING OPERATIONS ===================

;C FILL   c-addr u char --  fill memory with char
    head FILL,{"FILL"},docode
        ld a,c          ; character in a
        exx             ; use alt. register set
        pop bc          ; count in bc
        pop de          ; address in de
        or a            ; clear carry flag
        ld hl,ffffffh
        adc hl,bc       ; test for count=0 or 1
        jr nc,filldone  ;   no cy: count=0, skip
        ld (de),a       ; fill first byte
        jr z,filldone   ;   zero, count=1, done
        dec bc          ; else adjust count,
        ld hl,de        ;   let hl = start adrs,
        inc de          ;   let de = start adrs+1
        ldir            ;   copy (hl)->(de)
filldone: exx           ; back to main reg set
        pop bc          ; pop new TOS
        next

;X CMOVE   c-addr1 c-addr2 u --  move from bottom
; as defined in the ANSI optional String word set
; On byte machines, CMOVE and CMOVE> are logical
; factors of MOVE.  They are easy to implement on
; CPUs which have a block-move instruction.
    head CMOVE,{"CMOVE"},docode
        push bc
        exx
        pop bc      ; count
        pop de      ; destination adrs
		or	a
		ld	hl,0
		sbc	hl,bc	; test for count=0
        pop hl      ; source adrs
        jr z,cmovedone
        ldir        ; move from bottom to top
cmovedone: exx
        pop bc      ; pop new TOS
        next

;X CMOVE>  c-addr1 c-addr2 u --  move from top
; as defined in the ANSI optional String word set
    head CMOVEUP,{"CMOVE>"},docode
        push bc
        exx
        pop bc      ; count
		or	a
		ld	hl,0
		sbc	hl,bc	; test for count=0
        pop hl      ; destination adrs
        pop de      ; source adrs
        jr z,umovedone
        add hl,bc   ; last byte in destination
        dec hl
        ex de,hl
        add hl,bc   ; last byte in source
        dec hl
        lddr        ; move from top to bottom
umovedone: exx
        pop bc      ; pop new TOS
        next

;Z SKIP   c-addr u c -- c-addr' u'
;Z                          skip matching chars
; Although SKIP, SCAN, and S= are perhaps not the
; ideal factors of WORD and FIND, they closely
; follow the string operations available on many
; CPUs, and so are easy to implement and fast.
    head SKIP,{"SKIP"},docode
        ld a,c      ; skip character
        exx
        pop bc      ; count
		or	a
		ld	hl,0
		sbc	hl,bc	; test for count=0
        pop hl      ; address
        jr z,skipdone
skiploop:
;        EX  AF,AF'
;        EX  AF,AF'
        CPI
        jr nz,skipmis   ; char mismatch: exit
        jp pe,skiploop  ; count not exhausted
        jr skipdone     ; count 0, no mismatch
skipmis: inc bc         ; mismatch!  undo last to
        dec hl          ;  point at mismatch char
skipdone: push hl   ; updated address
        push bc     ; updated count
        exx
        pop bc      ; TOS in bc
        next

;Z SCAN    c-addr u c -- c-addr' u'
;Z                      find matching char
    head SCAN,{"SCAN"},docode
        ld a,c      ; scan character
        exx
        pop bc      ; count
		or	a
		ld	hl,0
		sbc	hl,bc	; test for count=0
        pop hl      ; address
        jr z,scandone
        cpir        ; scan 'til match or count=0
        jr nz,scandone  ; no match, BC & HL ok
        inc bc          ; match!  undo last to
        dec hl          ;   point at match char
scandone: push hl   ; updated address
        push bc     ; updated count
        exx
        pop bc      ; TOS in bc
        next

;Z S=    c-addr1 c-addr2 u -- n   string compare
;Z             n<0: s1<s2, n=0: s1=s2, n>0: s1>s2
    head SEQUAL,{"S="},docode
        push bc
        exx
        pop bc      ; count
		or	a
		ld	hl,0
		sbc	hl,bc	; test for count=0
        pop hl      ; addr2
        pop de      ; addr1
        jr z,smatch     ; by definition, match!
sloop:
        ld a,(de)
        inc de
        cpi
        jr nz,sdiff     ; char mismatch: exit
        jp pe,sloop     ; count not exhausted
smatch: ; count exhausted & no mismatch found
        exx
        ld bc,0         ; bc=0000  (s1=s2)
        jr snext
sdiff:  ; mismatch!  undo last 'cpi' increment
        dec hl          ; point at mismatch char
        cp (hl)         ; set cy if char1 < char2
		ld	bc,1
		jr	nc,$F
		dec bc
		dec bc
$$:		push bc		
        exx
		pop bc
snext:  next


 COMMENT %
;Z WRSEC   src dst -- src+256 dst+256
;Z   write 256-byte sector to AT29C040A flash
;    returned values are n
    head WRSEC,{"WRSEC"},docode
        LD  A,00h
        POP HL      ;set source address (src)
        PUSH DE     ; save IP (Instruction Pointer)
        LD  E,C     ;TOS into DE (dst)
        LD  D,B
        LD  BC,256  ;set count
        LD  A,0AAh ; .....
        LD  (5555h),A  ;write-enable algorithm
        LD  A,55h
        LD  (2AAAh),A
        LD  A,0A0h
        LD  (5555h),A
        LDIR        ;move 256 bytes to Flash ROM
        DEC HL      ;point back to last byte programmed
        DEC DE
 $$     LD  A,(DE)  ;compare last written byte,
        XOR (HL)    ;use only bit d7... it
        AND 80h     ;will toggle until end of
        JR  NZ,$B   ;  internal write
        INC HL      ; bump forward again
        INC DE      ;bump forward again
        LD  C,E     ;DE (dst) to TOS
        LD  B,D
        POP DE     ; restore IP (Instruction Pointer)
        PUSH HL    ; set 2nd stack item (src)
        next
 % ENDCOMMENT

	; We have to including all together into
	; one big file to preserve the link chain.
	INCLUDED EQU 1
	.INCLUDE "CAMLF91d.asm"   ; CPU Dependencies
    .INCLUDE "CAMLF91h.asm"   ; High Level words

	; HEADER STRUCTURE ==============================
	; The structure of the Forth dictionary headers
	; (name, link, immediate flag, and "smudge" bit)
	; does not necessarily differ across CPUs.  This
	; structure is not easily factored into distinct
	; "portable" words; instead, it is implicit in
	; the definitions of FIND and CREATE, and also in
	; NFA>LFA, NFA>CFA, IMMED?, IMMEDIATE, HIDE, and
	; REVEAL.  These words must be (substantially)
	; rewritten if either the header structure or its
	; inherent assumptions are changed.

lastword EQU link       ; nfa of last word in dict.

	segment bss

InpBuffer 	DS 128 
user	.tag	USERAREA
user:	
ifdef _DEBUG
dbg_U0 			DS	3		;  0 USER U0        current user area adrs
dbg_TOIN 		DS	3		;  3 USER >IN       holds offset into TIB
dbg_BASE		DS	3		;  6 USER BASE      holds conversion radix
dbg_STATE		DS	3		;  9 USER STATE     holds compiler state
dbg_DP			DS	3		; 12 USER DP        holds dictionary ptr
dbg_TICKSOURCE	DS	6		; 15 USER SOURCE    two cells: len, adrs
dbg_LATEST		DS	3		; 21 USER LATEST    last word in dict.
dbg_HP			DS	3		; 24 USER HP        HOLD pointer
dbg_LP			DS	3		; 28 USER LP        Leave-stack pointer
dbg_S0			DS	3		; 31 Parameter stack, grows down
dbg_HOLDP		DS  3		; 34 HOLD grows down
dbg_PAD			DS  3		; 37 PAD buffer grows up
dbg_L0         	DS  3		; 40 bottom of Leave stack grows up
dbg_R0         	DS  3		; 43 Return stack, grows down
else
		ds USERAREASZ
endif
enddict		DS	3			    ; user's code starts here	
	
	ORG USERSEGMENT

			DS	PARASTACKSZ	;128
__S0		; Parameter stack, 128B, grows down

			DS	HOLDSTACKSZ	;40
__HOLDP		; HOLD area, 40 bytes, grows down

__PAD		DS	PADBUFFERSZ	; PAD buffer, 88 bytes

__L0		DS	LEAVESTACKSZ; 97 bottom of Leave stack grows up

			DS  RETNSTACKSZ	; 128
__R0		; Return stack, 128 B, grows down

        END     ;ENTRY
