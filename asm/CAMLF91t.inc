

TRACE_WORD MACRO name
ifdef _TRACE
	push bc
	push de
	push hl
	ld	hl,name
	xref trace_word
	call trace_word
	pop	hl
	pop de
	pop bc
endif ;_TRACE
	ENDMACRO
	

TRACE_NEXT MACRO
ifdef _TRACE
	push bc
	push de
	push hl
	xref trace_next
	call trace_next
	pop	hl
	pop de
	pop bc
endif ;_TRACE
	ENDMACRO
	
	