;*****************************************************************************
; vectors24.asm
;
; eZ80's Reset, RST and 2nd generation interrupt arrangement
;*****************************************************************************
; Copyright (C) 2005 by ZiLOG, Inc.  All Rights Reserved.
;*****************************************************************************

        XREF __init

        XDEF _reset
        XDEF __default_nmi_handler
        XDEF __default_mi_handler
        XDEF __nvectors
        XDEF _init_default_vectors
        XDEF __init_default_vectors
        XDEF _set_vector
        XDEF __set_vector
        XDEF __vector_table

NVECTORS EQU 64                ; number of potential interrupt vectors

; Save Interrupt State
SAVEIMASK MACRO
    ld a, i                    ; sets parity bit to value of IEF2
    push af
    di                         ; disable interrupts while loading table 
    MACEND

; Restore Interrupt State
RESTOREIMASK MACRO
    pop af
    jp po, $+5                 ; parity bit is IEF2
    ei
    MACEND


;*****************************************************************************
; Reset and all RST nn's
;  1. diaable interrupts
;  2. clear mixed memory mode (MADL) flag
;  3. jump to initialization procedure with jp.lil to set ADL
        DEFINE .RESET, SPACE = ROM
        SEGMENT .RESET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_reset:
_rst0:
    di
    rsmix
    jp.lil __init
_rst8:
    di
    rsmix
    jp.lil __init
_rst10:
    di
    rsmix
    jp.lil __init
_rst18:
    di
    rsmix
    jp.lil __init
_rst20:
    di
    rsmix
    jp.lil __init
_rst28:
    di
    rsmix
    jp.lil __init
_rst30:
    di
    rsmix
    jp.lil __init
_rst38:
    di
    rsmix
    jp.lil __init
        DS %26
_nmi:
    jp.lil __default_nmi_handler


;*****************************************************************************
; Startup code
        DEFINE .STARTUP, SPACE = ROM
        SEGMENT .STARTUP       ; This should be placed properly
        .ASSUME ADL=1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; number of vectors supported
__nvectors:
        DW NVECTORS            ; extern unsigned short _num_vectors;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Default Non-Maskable Interrupt handler
__default_nmi_handler:
    retn

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Default Maskable Interrupt handler
__default_mi_handler:
    ei
    reti

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialize all potential interrupt vector locations with a known
; default handler.
;
; void _init_default_vectors(void);
__init_default_vectors:
_init_default_vectors:
    push af
    SAVEIMASK
    ld hl, __default_mi_handler
    ld a, 0
    ld (__vector_table), hl    ; load default maskable irq handler
    ld (__vector_table + 3), a ; load a one byte filler
    ld hl, __vector_table
    ld de, __vector_table + 4
    ld bc, NVECTORS * 4 - 4
    ldir
    im 2                       ; interrtup mode 2
    ld hl, __vector_table >> 8
    ld i, hl                   ; load interrupt vector base
    RESTOREIMASK
    pop af
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Installs a user interrupt handler in the vector table
;
; void * _set_vector(unsigned int vector, void(*handler)(void));
__set_vector:
_set_vector:
    push iy
    ld iy, 0                   ; standard prologue
    add iy, sp
    push af
    SAVEIMASK
    ld hl, (iy+6)              ; load vector offset
    ld bc, __vector_table      ; load base address for vector table
    add hl, bc                 ; calculate vector location
    ld bc, (iy+9)              ; handler
    ld de, (hl)                ; save previous handler
    ld (hl), bc                ; store new vector address
    push de
    pop hl                     ; return previous handler
    RESTOREIMASK
    pop af
    ld sp, iy                  ; standard epilogue
    pop iy
    ret


;*****************************************************************************
; This segment must be aligned on a 512 byte boundary anywhere in RAM
; Each entry will be a 3-byte address in a 4-byte space 
        DEFINE .IVECTS, SPACE = RAM, ALIGN = 200h
        SEGMENT .IVECTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
__vector_table:
        DS NVECTORS * 4


        END
