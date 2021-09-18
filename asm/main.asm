	.include "bsp.inc"
	
	;forth program ENTRY POINT
	xref ENTRY
	
		segment TEXT

BUFSZ	macro	sz
		db	BUFSZ&sz	; bufsz register constant
		db	sz-1		; bufalign
	endmacro BUFSZ
	
	xdef emaccfg 
emaccfg		.tag	EMACCFG
emaccfg:
			dw24	#(__RAM_ADDR_U_INIT_PARAM << 16) + C000h	; tlbp Transmit Lower Boundary Pointer
			dw24	#(__RAM_ADDR_U_INIT_PARAM << 16) + D000h	; bp Boundary Pointer
			dw24	#(__RAM_ADDR_U_INIT_PARAM << 16) + E000h	; rhbp Receive High Boundary Pointer
			db 		00h,90h,23h,00h,01h,01h						; macaddr
			BUFSZ 32
	; end emaccfg

BAUDRATE0 	EQU 115200 ;57600 ;38400

	xdef	uart0cfg
uart0cfg	.tag	UARTCFG
uart0cfg:
			dw		((_SYS_CLK_FREQ / BAUDRATE0) / 16)	; divisor baudrate 115200
			db  	LCTL_CHAR_8_1						; lctl line control 8,1,n

	segment CODE
	.assume adl=1
	xdef	_main
_main:	
;			ld		iy,bspcfg
			call	init_bsp
			
			ld		iy,uart0cfg
			call	init_uart0
		
			ld		iy,emaccfg
;			call	init_emac

			jp		ENTRY
	
	END