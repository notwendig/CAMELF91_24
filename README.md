# CAMELF91_24

24-bit CamelForth for the **Zilog eZ80F91** development kit, with native ZDS II
configurations, three-button input and a 5×7 LED matrix.

## Requirements and setup

- Zilog Developer Studio II eZ80Acclaim! **5.3.5**.
- Nested `libbsp/` submodule.
- An eZ80F91 development kit or matching QEMU environment.
- UART0 at 115200 baud, 8N1; the native driver uses RTS/CTS.

```sh
git clone https://github.com/notwendig/CAMELF91_24.git
cd CAMELF91_24
git submodule update --init -- libbsp
```

The BSP revision is available through public HTTPS; no local SSH alias is required.

## Build and run

1. Open `libbsp/libbsp.zdsproj`; build **Debug → Rebuild All**, producing `libbspd.lib`.
2. Open `camel_f91.zdsproj`, select **Debug** and rebuild.
3. Select the board target/debugger, load `Debug/camel_f91.lod` and press **F5**.
4. Enter Forth commands over UART0 after the banner.

The maintained Debug configuration uses Ethernet Smart Cable. Physical targets
may select USB Smart Cable. The RAM target disables external flash detection:
with CS0 disabled, AMD probes beginning at 100000h cannot succeed.

After ZDS generates its makefile, Linux/Wine builds are also supported:

```sh
make
make ZDS_HOME=/path/to/ZDSII_eZ80Acclaim!_5.3.5
make clean
```

`BUILD` selects the configuration, default Debug. The wrapper preserves build
failure status despite filtering output. Generated `.mak`/`.linkcmd` files are
not tracked; project/target files are authoritative.

## Configurations

| Configuration | Code | Data |
| --- | --- | --- |
| Debug | External SRAM 000000h–07FFFFh | Internal RAM from B7D000h and external RAM to BFFFFFh |
| Release | Internal flash | Internal/external RAM per linker settings |
| Chiponly | Internal flash | Internal RAM per linker settings |
| DebugTrace | Debug RAM with trace code | Per DebugTrace linker settings |

Debug is the thoroughly tested emulator profile. Other configurations require
validation against their actual hardware and target settings.

## Forth and kit

```forth
1 2 + .
: SQUARE DUP * ;
12 SQUARE .
BUTTONS .
1 BUTTON? .
CHAR A MATRIX-CHAR
MATRIX-OFF
```

| Word | Meaning |
| --- | --- |
| `BUTTONS` | Mask 0–7: SW1=1, SW2=2, SW3=4 |
| `BUTTON?` | Button 1–3; -1 pressed, otherwise 0 |
| `SW1?` `SW2?` `SW3?` | Direct button queries |
| `MATRIX-CHAR` | ASCII 32–126; starts Timer1 scanning |
| `MATRIX!` | Copy seven bitmap row bytes |
| `MATRIX-CLEAR` | Clear image, keep scanning |
| `MATRIX-OFF` | Clear image and stop Timer1 |

Each row uses five active-high bits; bit 4 is left. Button queries are instantaneous
without debouncing. Timer1 belongs to the matrix while scanning. `BUTTON-INIT`
and `MATRIX-INIT` provide explicit initialization.

## Sources and corrections

- `asm/CAMLF91e.asm`: primitives and native BSP/UART startup.
- `asm/CAMLF91d.asm`: cell width, code fields and compiler helpers.
- `asm/CAMLF91h.asm`: high-level words and kit inclusion.
- `asm/kit.inc`, `asm/kit_font.inc`: buttons, Timer1 matrix and font.
- `asm/cstartup.asm`, `asm/init_params_f91.asm`, `asm/vectors24.asm`: startup.
- `libbsp/`: separate assembly library.

The core corrects DEPTH, FM/MOD, M+, ACCEPT and D.R/.R sign formatting.
Native startup explicitly initializes UART0 before output. Define `KIT` to
include kit words in the native assembly configuration.

## Emulator tests

The [qemuez80f91 workspace](https://github.com/notwendig/qemuez80f91) provides a
polling build, 89 before/after-reset cases, 237-word coverage with 201 checks,
and kit tests. Its builder reads corrected commit `af47e65` directly and creates
output outside this checkout; it does not apply separate workspace core patches.

Original notices remain in [ReadMe.txt](ReadMe.txt), [LICENSE](LICENSE) and source
headers. CamelForth's original personal, educational and commercial usage terms
remain authoritative; this README does not replace them.
