
First step, Camel 24bit Forth for the eZ80 Acclaim! eZ80F91 -- have fun!
    www.NadiSoft.de <JSievers@NadiSoft.de>

This port originated from the web pages of Douglas Beattie Jr.
 http://www.hytherion.com/beattidp/


About this port of Camel Forth:

This is a ZDSII_eZ80Acclaim!_5.3.4 project, which contains two separate builds:
See https://www.zilog.com zds2_eZ80Acclaim!_5.3.4_19112104.zip
These projects are configured to build for the eZ80F91 Development Board
Revision 99C0858-001 Rev C or later.

 - One build is 'Debug', which runs entirely out of 512KB eZ80F91 Module RAM
    at 000000-03FFFFh for code (read only RAM) and 040000h-07FFFFh date RAM
   directly from ZDS-II.

 - The other is 'Release', which generates a HEX file, to burn into
   the eZ80F91 on-chip Flash with the Flash Loader (found in the Tools
   menu of ZDS II). ROM 000000h-03FFFFh and RAM B7D000-B7FFFFh.

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
