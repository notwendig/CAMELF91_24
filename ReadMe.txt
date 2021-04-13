
First step, Camel 24bit Forth for the eZ80 Acclaim! eZ80F91 -- have fun!
    www.NadiSoft.de <JSievers@NadiSoft.de>

This port originated from the web pages of Douglas Beattie Jr.
 http://www.hytherion.com/beattidp/


About this port of Camel Forth:

This is a ZDSII_eZ80Acclaim!_5.3.4 project, which contains three separate builds:
See https://www.zilog.com zds2_eZ80Acclaim!_5.3.4_19112104.zip
These projects are configured to build for the eZ80F91 Development Board
Revision 99C0858-001 Rev C or later.

 - One build is 'Debug', which runs entirely out of 512KB eZ80F91 Module SRAM
    as ROM mapped at 000000-07FFFFh for code and Chip's SRAM B7D000h-B7FFFFh continued
    by DevPlattform's B80000h-BFFFFFh data SRAM
   directly from ZDS-II.

eZ80F91 Camel FORTH 1.00e (24bit) - Debug, 13 APR 2021
MEMCFG
TIB:       B7D100 124
U0:        B7D180 B7D180
>IN:       B7D183 6
BASE:      B7D186 10
STATE:     B7D189 0
DP:        B7D18C B7D1AD
SOURCE:    B7D18F 6 B7D100
LATEST:    B7D195 230E
HP:        B7D198 BFFE21
LP:        B7D19B BFFE7F
S0:        B7D19E BFFDD7
HOLD:      B7D1A1 BFFE27
PAD:       B7D1A4 BFFE27
L0:        B7D1A7 BFFE7F
R0:        B7D1AA BFFFFF

 - The other is 'Release', which generates a HEX file, to burn into
   the eZ80F91 on-chip Flash with the Flash Loader (found in the Tools
   menu of ZDS II). Chip's ROM 000000h-03FFFFh and Chip's SRAM B7D000h-B7FFFFh continued
   by DevPlattform's B80000h-BFFFFFh and Module's C00000h-C7FFFFh data SRAM

eZ80F91 Camel FORTH 1.00e (24bit) - Release, 13 APR 2021
MEMCFG
TIB:       B7D100 124
U0:        B7D180 B7D180
>IN:       B7D183 6
BASE:      B7D186 10
STATE:     B7D189 0
DP:        B7D18C B7D1AD
SOURCE:    B7D18F 6 B7D100
LATEST:    B7D195 2310
HP:        B7D198 C7FE21
LP:        B7D19B C7FE7F
S0:        B7D19E C7FDD7
HOLD:      B7D1A1 C7FE27
PAD:       B7D1A4 C7FE27
L0:        B7D1A7 C7FE7F
R0:        B7D1AA C7FFFF

 - The 3trh is 'Chipony', which generates a HEX file, to burn into
   the eZ80F91 on-chip Flash with the Flash Loader (found in the Tools
   menu of ZDS II). Chip's Flash 000000h-03FFFFh and SRAM B7D000-B7FFFFh.

 eZ80F91 Camel FORTH 1.00e (24bit) - Chiponly, 13 APR 2021
MEMCFG
TIB:       B7D100 124
U0:        B7D180 B7D180
>IN:       B7D183 6
BASE:      B7D186 10
STATE:     B7D189 0
DP:        B7D18C B7D1AD
SOURCE:    B7D18F 6 B7D100
LATEST:    B7D195 2311
HP:        B7D198 B7FE21
LP:        B7D19B B7FE7F
S0:        B7D19E B7FDD7
HOLD:      B7D1A1 B7FE27
PAD:       B7D1A4 B7FE27
L0:        B7D1A7 B7FE7F
R0:        B7D1AA B7FFFF

Select the build using 'Set Active Configuration' from the 'Build' menu,
or from the toolbar.

The HEX file is included -- you can burn the Flash, press reset, and
you're running.

If you plan to embed this into a commercial product (for manufacturing test,
in-lab use, field diagnostics, etc.), please contact the author for a
very reasonable licensing deal, e.g. per board, or unlimited distribution.

The Author:  Bradford J. Rodriguez, Ph.D. -- http://www.zetetics.com/bj/
Port to 24Bit 1.4.2021 by: Jürgen Willi Sievers, http://NadiSoft.de


CamelForth is NOT freeware. All rights are reserved.

Personal or Educational Use: Permission is granted to freely copy, modify,
and distribute CamelForth for personal or educational use. You're welcome
to use it, change it, experiment with it, give it to friends, teach it to
your students -- anything EXCEPT make money from it, directly or indirectly.

Commercial Use: If you want to sell CamelForth, include it as part of
something you're selling, or base a product upon it, you must negotiate a
commercial license. The default license terms are US$2.00 (two dollars U.S.)
per copy sold. If you expect to sell many copies, you may wish to negotiate
an unlimited distribution license. Send email to bj@zetetics.com to inquire.

( http://www.zetetics.com/camel/ )
same for the 24Bit-Port
( http://NadiSoft.de mailto:JSievers@NadiSoft.de )
