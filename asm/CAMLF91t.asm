
	.list off
    .INCLUDE "eZ80F91.INC"    ; CPU Equates
	.INCLUDE "intvect.inc"
	.list on
	.INCLUDE "camlf91.inc"

	xref TXMCHR	; transmit byte in reg C

	SEGMENT CODE
	.ASSUME ADL=1

link	.SET CAMEL91H_LAST

ifdef _TRACE

	xdef trace_word
trace_word:
	ld		a,(margine)
	inc		a
	jr		z,$F
	ld		(margine),a
$$:	xor		a,a
	ld		(trace_off),a
	call	trace0
	call	trace_out
	ret

	xdef trace_next
trace_next:
	ld		a,(margine)
	or		a,a
	jr		z,$F
	dec		a
	ld		(margine),a
$$:	ret

trace0:	; hl = ptr name
	dec		hl
	ld		a,'0'
	add		a,(hl)	;'1' then immed
	inc		hl
	ld		b,(hl)
$$:	inc 	hl
	ld		a,(hl)
	call	trace_putc
	djnz	$B
	ret

trace_putc:
	push	hl
	ld		hl,(trace_off)
	ld		(hl),a
	ld		a,l
	inc		a
	jr		z,$F
	ld		(trace_off),a
$$:	pop		hl
	ret

Num2Dec:
	push 	de
	ld		e,'0'
	call	$F
	ld		c,':'
	call	TXMCHR
	pop		de
	ret
	
$$:	ld	bc,-1000000
	call	Num1
	ld	bc,-10000
	call	Num1
	ld	bc,-10000
	call	Num1
	ld	bc,-1000
	call	Num1
	ld	bc,-100
	call	Num1
	ld	c,-10
	call	Num1
	ld	c,b

Num1:
	ld	a,'0'-1
Num2:
	inc	a
	add	hl,bc
	jr	c,Num2
	sbc	hl,bc
	ld		c,' '
	cp		a,e
	jr		z,$F
	ld		c,a
	dec		e
$$:	call	TXMCHR
	ret

trace_out:
	push	bc
	ld		hl,(tracelcnt)
	inc		hl
	ld		(tracelcnt),hl
	call	Num2Dec
	ld		a,(margine)
	or		a,a
	jr		z,$endmargine

	ld		b,a
	ld		c,'-'
$$:	call	TXMCHR
	djnz	$B
$endmargine:
	ld		hl,(trace_off)
	ld		a,l
	or		a,a
	jr		z,$endline
	ld		b,a
	ld		l,0
$$:	ld		c,(hl)
	call	TXMCHR
	inc		hl
	djnz	$B
	xor		a,a
	ld		(trace_off),a
$endline:
	ld		c,0dh
	call	TXMCHR
	ld		c,0ah
	call	TXMCHR
	pop		bc
	ret


	segment data
tracelcnt:	DW24	0
margine:	DB		0
trace_off:	DW24	trace_line

	segment bss

	.align 100h
trace_line:	DS 100h

else
	nop
endif ;_TRACE

	XDEF CAMLF91T_LAST
CAMLF91T_LAST EQU link

	END
