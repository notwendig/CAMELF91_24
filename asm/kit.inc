; Development-kit words for SW1..3 and the multiplexed 5x7 LED matrix.
; Timer1 is owned by the matrix while enabled. Preserve all Forth registers.
; Included after the core headers in CAMLF91h.asm.

KIT_SAVE MACRO
    ld a,i
    push af
    di
    ENDMACRO KIT_SAVE
KIT_RESTORE MACRO
    pop af
    jp po,$+5
    ei
    ENDMACRO KIT_RESTORE

KIT_RELOAD EQU (_SYS_CLK_FREQ / 16 / 700)

    SEGMENT CODE
    .ASSUME ADL=1

; BUTTON-INIT ( -- ) PB0..2 as plain inputs; leave PB3..7 unchanged.
    head BUTTON_INIT,{"BUTTON-INIT"},docode
    call kit_buttons_init
    next
kit_buttons_init:
    in0 a,(09bh)
    or 7
    out0 (09bh),a
    in0 a,(09ch)
    and 0f8h
    out0 (09ch),a
    in0 a,(09dh)
    and 0f8h
    out0 (09dh),a
    ret
kit_read_buttons:
    call kit_buttons_init
    in0 a,(09ah)
    cpl
    and 7
    ret

; BUTTONS ( -- mask ) bit 0=SW1, bit 1=SW2, bit 2=SW3; pressed=1.
    head BUTTONS,{"BUTTONS"},docode
    push bc
    call kit_read_buttons
    ld bc,0
    ld c,a
    next

; BUTTON? ( n -- flag ) 1=left, 2=middle, 3=right; invalid n gives false.
    head BUTTON_QUERY,{"BUTTON?"},docode
    ld hl,bc
    dec hl
    push hl
    ld bc,3
    or a
    sbc hl,bc
    pop hl
    ld bc,0
    jr nc,kit_button_done
    ld b,l
    call kit_read_buttons
    inc b
kit_button_shift:
    dec b
    jr z,kit_button_test
    rrca
    jr kit_button_shift
kit_button_test:
    and 1
    ld bc,0
    jr z,kit_button_done
    dec bc
kit_button_done:
    next

    head SW1_QUERY,{"SW1?"},docolon
    DW24 LIT,1,BUTTON_QUERY,EXIT
    head SW2_QUERY,{"SW2?"},docolon
    DW24 LIT,2,BUTTON_QUERY,EXIT
    head SW3_QUERY,{"SW3?"},docolon
    DW24 LIT,3,BUTTON_QUERY,EXIT

; MATRIX-INIT ( -- ) start a 700-row/s Timer1 scan, initially blank.
    head MATRIX_INIT,{"MATRIX-INIT"},docode
    call kit_matrix_init
    next
kit_matrix_init:
    push af
    ld a,(kit_matrix_enabled)
    or a
    jr nz,kit_matrix_init_done
    push bc
    push de
    push hl
    KIT_SAVE
    xor a
    out0 (065h),a
    out0 (066h),a
    in0 a,(067h)
    call kit_matrix_blank
    ld hl,kit_matrix_isr
    push hl
    ld hl,058h
    push hl
    call _set_vector
    pop hl
    pop hl
    ld a,1
    ld (kit_matrix_enabled),a
    xor a
    ld (kit_matrix_row),a
    in0 a,(0dch)
    and 0fdh
    out0 (0dch),a
    ld hl,KIT_RELOAD
    ld a,h
    out0 (069h),a
    ld a,l
    out0 (068h),a
    ld a,1
    out0 (066h),a
    ld a,08fh
    out0 (065h),a
    KIT_RESTORE
    pop hl
    pop de
    pop bc
kit_matrix_init_done:
    pop af
    ret

; MATRIX-CHAR ( char -- ) ASCII 32..126; other values display '?'.
; Starts the background scan automatically. Lower-case glyphs match capitals.
    head MATRIX_CHAR,{"MATRIX-CHAR"},docode
    push de
    ld hl,bc
    ld de,32
    or a
    sbc hl,de
    push hl
    ld de,95
    or a
    sbc hl,de
    pop hl
    jr c,kit_char_valid
    ld hl,31
kit_char_valid:
    ld de,hl
    add hl,hl
    add hl,hl
    add hl,hl
    or a
    sbc hl,de
    ld de,kit_font
    add hl,de
    call kit_matrix_copy
    pop de
    pop bc
    next

; MATRIX! ( addr -- ) copy seven rows; low five bits are active-high pixels.
    head MATRIX_STORE,{"MATRIX!"},docode
    ld hl,bc
    call kit_matrix_copy
    pop bc
    next
kit_matrix_copy:
    call kit_matrix_init
    push bc
    push de
    KIT_SAVE
    ld de,kit_matrix_pixels
    ld bc,7
    ldir
    KIT_RESTORE
    pop de
    pop bc
    ret

; MATRIX-CLEAR ( -- ) blank the image while retaining background refresh.
    head MATRIX_CLEAR,{"MATRIX-CLEAR"},docode
    call kit_matrix_init
    push bc
    push hl
    KIT_SAVE
    call kit_matrix_blank
    KIT_RESTORE
    pop hl
    pop bc
    next

; MATRIX-OFF ( -- ) stop Timer1, blank the matrix, release no other timer.
    head MATRIX_OFF,{"MATRIX-OFF"},docode
    push bc
    push hl
    KIT_SAVE
    xor a
    out0 (066h),a
    out0 (065h),a
    in0 a,(067h)
    xor a
    ld (kit_matrix_enabled),a
    call kit_matrix_blank
    KIT_RESTORE
    pop hl
    pop bc
    next

kit_matrix_blank:
    xor a
    ld (0800000h),a
    ld hl,kit_matrix_pixels
    ld b,7
kit_blank_loop:
    ld (hl),a
    inc hl
    djnz kit_blank_loop
    ld a,01fh
    ld (0800001h),a
    ret

; Do not use alternate registers or modify the Forth IP/return/user pointers.
; Blank anodes before changing columns to prevent row-switch ghost pixels.
    XDEF kit_matrix_isr
kit_matrix_isr:
    push af
    push bc
    push hl
    in0 a,(067h)
    xor a
    ld (0800000h),a
    ld bc,0
    ld a,(kit_matrix_row)
    ld c,a
    ld hl,kit_matrix_pixels
    add hl,bc
    ld a,(hl)
    xor 01fh
    and 01fh
    ld (0800001h),a
    ld hl,kit_row_bits
    add hl,bc
    ld a,(hl)
    ld (0800000h),a
    ld a,c
    inc a
    cp 7
    jr c,kit_next_row
    xor a
kit_next_row:
    ld (kit_matrix_row),a
    pop hl
    pop bc
    pop af
    ei
    reti

    SEGMENT TEXT
kit_row_bits:
    DB 1,2,4,8,16,32,64
    .INCLUDE "kit_font.inc"

    SEGMENT BSS
    XDEF kit_matrix_pixels
kit_matrix_pixels: DS 7
kit_matrix_row: DS 1
kit_matrix_enabled: DS 1
    SEGMENT CODE
