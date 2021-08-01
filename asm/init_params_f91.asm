;*****************************************************************************
; init_params_f91.asm
;
; minimum eZ80F91 initialization
;*****************************************************************************
; Copyright (C) 2005 by ZiLOG, Inc.  All Rights Reserved.
;*****************************************************************************

        INCLUDE "ez80f91.inc"

        
        XREF __stack
        XREF __init_default_vectors
        XREF __c_startup
        XREF __cstartup
        XREF _main
        XREF __CS0_LBR_INIT_PARAM
        XREF __CS0_UBR_INIT_PARAM
        XREF __CS0_CTL_INIT_PARAM
        XREF __CS1_LBR_INIT_PARAM
        XREF __CS1_UBR_INIT_PARAM
        XREF __CS1_CTL_INIT_PARAM
        XREF __CS2_LBR_INIT_PARAM
        XREF __CS2_UBR_INIT_PARAM
        XREF __CS2_CTL_INIT_PARAM
        XREF __CS3_LBR_INIT_PARAM
        XREF __CS3_UBR_INIT_PARAM
        XREF __CS3_CTL_INIT_PARAM
        XREF __CS0_BMC_INIT_PARAM
        XREF __CS1_BMC_INIT_PARAM
        XREF __CS2_BMC_INIT_PARAM
        XREF __CS3_BMC_INIT_PARAM
        XREF __FLASH_CTL_INIT_PARAM
        XREF __FLASH_ADDR_U_INIT_PARAM
        XREF __RAM_CTL_INIT_PARAM
        XREF __RAM_ADDR_U_INIT_PARAM

        XDEF __init
        XDEF _abort
        XDEF __exit
        XDEF _exit


;*****************************************************************************
; Startup code
        DEFINE .STARTUP, SPACE = ROM
        SEGMENT .STARTUP
        .ASSUME ADL = 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Minimum default initialization for eZ80F91
__init:
    ; disable internal peripheral interrupt sources
    ; -- this will help during a RAM debug session --
    ld a, %FF
    out0 (PA_DDR), a         ; GPIO
    out0 (PB_DDR), a         ;
    out0 (PC_DDR), a         ;
    out0 (PD_DDR), a         ;
    ld a, %00
    out0 (PA_ALT1), a        ;
    out0 (PB_ALT1), a        ;
    out0 (PC_ALT1), a        ;
    out0 (PD_ALT1), a        ;
    out0 (PA_ALT2), a        ;
    out0 (PB_ALT2), a        ;
    out0 (PC_ALT2), a        ;
    out0 (PD_ALT2), a        ;
    out0 (PLL_CTL1), a       ; PLL
    out0 (TMR0_IER), a       ; timers
    out0 (TMR1_IER), a       ;
    out0 (TMR2_IER), a       ;
    out0 (TMR3_IER), a       ;
    out0 (UART0_IER), a      ; UARTs
    out0 (UART1_IER), a      ;
    out0 (I2C_CTL), a        ; I2C
    out0 (EMAC_IEN), a       ; EMAC
    out0 (FLASH_IRQ), a      ; Flash
    ld a, %04
    out0 (SPI_CTL), a        ; SPI
    in0 a, (RTC_CTRL)        ; RTC, Writing to the RTC_CTRL register also
    and a, %BE               ;      resets the RTC count prescaler allowing
    out0 (RTC_CTRL), a       ;      the RTC to be synchronized to another
                             ;      time source. 

    ; configure external memory/io
    ld a, __CS0_LBR_INIT_PARAM
    out0 (CS0_LBR), a
    ld a, __CS0_UBR_INIT_PARAM
    out0 (CS0_UBR), a
    ld a, __CS0_BMC_INIT_PARAM
    out0 (CS0_BMC), a
    ld a, __CS0_CTL_INIT_PARAM
    out0 (CS0_CTL), a

    ld a, __CS1_LBR_INIT_PARAM
    out0 (CS1_LBR), a
    ld a, __CS1_UBR_INIT_PARAM
    out0 (CS1_UBR), a
    ld a, __CS1_BMC_INIT_PARAM
    out0 (CS1_BMC), a
    ld a, __CS1_CTL_INIT_PARAM
    out0 (CS1_CTL), a

    ld a, __CS2_LBR_INIT_PARAM
    out0 (CS2_LBR), a
    ld a, __CS2_UBR_INIT_PARAM
    out0 (CS2_UBR), a
    ld a, __CS2_BMC_INIT_PARAM
    out0 (CS2_BMC), a
    ld a, __CS2_CTL_INIT_PARAM
    out0 (CS2_CTL), a

    ld a, __CS3_LBR_INIT_PARAM
    out0 (CS3_LBR), a
    ld a, __CS3_UBR_INIT_PARAM
    out0 (CS3_UBR), a
    ld a, __CS3_BMC_INIT_PARAM
    out0 (CS3_BMC), a
    ld a, __CS3_CTL_INIT_PARAM
    out0 (CS3_CTL), a

    ; enable internal memory
    ld a, __FLASH_ADDR_U_INIT_PARAM
    out0 (FLASH_ADDR_U), a
    ld a, __FLASH_CTL_INIT_PARAM
    out0 (FLASH_CTRL), a

    ld a, __RAM_ADDR_U_INIT_PARAM
    out0 (RAM_ADDR_U), a
    ld a, __RAM_CTL_INIT_PARAM
    out0 (RAM_CTL), a

    ; setup Stack Pointer
    ld sp, __stack

    ; initialize the interrupt vector table
    call __init_default_vectors

    ; Initialize Clock
    call _InitSysClk

    ; start application
    ld a, __cstartup
    or a, a
    jr z, __no_cstartup
    call __c_startup

__no_cstartup:
    ;--------------------------------------------------
    ; Initialize the peripheral devices

        XREF __open_periphdevice

    call __open_periphdevice

    ;---------------------------------------------
    ; prepare to go to the main system rountine
    ld hl, 0                   ; hl = NULL
    push hl                    ; argv[0] = NULL
    ld ix, 0
    add ix, sp                 ; ix = &argv[0]
    push ix                    ; &argv[0]
    pop hl
    ld de, 0                   ; argc = 0
    call _main                 ; int main(int argc, char *argv[]))
    pop de                     ; clean the stack

__exit:
_exit:
_abort:
    ;--------------------------------------------------
    ; Close the peripheral devices

        XREF __close_periphdevice

    call __close_periphdevice

    jr $                ; if we return from main loop forever here



;*****************************************************************************
; eZ80F91 System Clock Initialization
;*****************************************************************************

;PLL_DIV_L     EQU %5C
;PLL_DIV_H     EQU %5D
;PLL_CTL0      EQU %5E
;PLL_CTL1      EQU %5F


OSC           EQU 0
PLL           EQU 1
RTC           EQU 2

CLK_MUX_OSC   EQU %00
CLK_MUX_PLL   EQU %01
CLK_MUX_RTC   EQU %02

CHRP_CTL_0    EQU %00
CHRP_CTL_1    EQU %40
CHRP_CTL_2    EQU %80
CHRP_CTL_3    EQU %C0

LDS_CTL_0     EQU %00
LDS_CTL_1     EQU %04
LDS_CTL_2     EQU %08
LDS_CTL_3     EQU %0C

LCK_STATUS    EQU %20
INT_LOCK      EQU %10
INT_UNLOCK    EQU %08
INT_LOCK_EN   EQU %04
INT_UNLOCK_EN EQU %02
PLL_ENABLE    EQU %01

        XREF _SYS_CLK_SRC
        XREF _SYS_CLK_FREQ
        XREF _OSC_FREQ
        XREF _OSC_FREQ_MULT
        XREF __PLL_CTL0_INIT_PARAM


        ;SEGMENT STARTUP
        ;.ASSUME ADL=1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_init_sys_clk_pll:
    ; load PLL divider
    ld a, (_OscFreqMult)		;CR 6202
    out0 (PLL_DIV_L), a
    ld a, (_OscFreqMult+1)
    out0 (PLL_DIV_H), a
    
	; Set charge pump and lock criteria
    ld a, __PLL_CTL0_INIT_PARAM
    and a, %CC  ; mask off reserved and clock source bits
    out0 (PLL_CTL0), a
    ; enable PLL
    in0 a, (PLL_CTL1)
    set 0, a
    out0 (PLL_CTL1), a
    ; wait for PLL to lock
while_no_lock:
    in0 a, (PLL_CTL1)
    and a, LCK_STATUS
    cp a, LCK_STATUS
    jr nz, while_no_lock
    ; select PLL as system clock source
    ld a, __PLL_CTL0_INIT_PARAM
    set 0, a
    out0 (PLL_CTL0), a
    ; return
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_InitSysClk:
    ; check if the PLL should be used
    ld a, (_SysClkSrc)
    cp a, PLL
    jr nz, _InitSysClkDone
    call _init_sys_clk_pll
_InitSysClkDone:
    ; return
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_OscFreq:
        DL _OSC_FREQ
_OscFreqMult:
        DW _OSC_FREQ_MULT
_SysClkFreq:
        DL _SYS_CLK_FREQ
_SysClkSrc:
        DB _SYS_CLK_SRC



        XDEF _InitSysClk
        XDEF _InitSysClkDone
        XDEF _init_sys_clk_pll
        XDEF _OscFreq
        XDEF _SysClkFreq
        XDEF _SysClkSrc

        END
