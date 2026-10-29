# CAMELF91_24

24-Bit-CamelForth für den **Zilog eZ80F91** und das eZ80F91 Development Kit.
Das Projekt enthält die nativen ZDS-II-Konfigurationen sowie Forth-Wörter
für die drei Taster und die 5×7-LED-Matrix.

## Voraussetzungen

- Zilog Developer Studio II eZ80Acclaim! **5.3.5**;
- das verschachtelte Submodul `libbsp/`;
- ein eZ80F91 Development Kit oder die passende QEMU-eZ80F91-Umgebung;
- UART0 mit 115200 Baud, 8N1; der native BSP-Treiber verwendet RTS/CTS.

```sh
git clone https://github.com/notwendig/CAMELF91_24.git
cd CAMELF91_24
git submodule update --init -- libbsp
```

Der BSP-Commit ist über die öffentliche HTTPS-Quelle erreichbar; eine lokale
SSH-Hostdefinition wird nicht benötigt.

## Bauen und starten

1. `libbsp/libbsp.zdsproj` in ZDS II öffnen und **Debug → Rebuild All** bauen.
   Das Ergebnis heißt `libbspd.lib` im BSP-Verzeichnis.
2. `camel_f91.zdsproj` öffnen, **Debug** auswählen und neu bauen.
3. Das passende Board-Target und den Debugger wählen, `Debug/camel_f91.lod`
   laden und mit **F5** fortsetzen.
4. Nach dem Banner Befehle über UART0 eingeben.

Der Projektstand verwendet für Debug Ethernet Smart Cable. Auf realer
Hardware kann im ZDS-Debugger-Setup wieder USB Smart Cable gewählt werden.
Das RAM-Debug-Target deaktiviert die externe Flash-Erkennung, da diese bei
abgeschaltetem CS0 zu erfolglosen AMD-Flash-Proben ab 100000h führt.

Unter Linux mit Wine kann nach Erstellung der ZDS-Make-Datei auch gebaut werden:

```sh
make
make ZDS_HOME=/pfad/zu/ZDSII_eZ80Acclaim!_5.3.5
make clean
```

`BUILD` wählt die Konfiguration; Standard ist `Debug`. Der Wrapper reicht
Buildfehler trotz Ausgabefilterung zuverlässig an den Aufrufer weiter.
Die `.mak`-/`.linkcmd`-Dateien in den Konfigurationsverzeichnissen werden von
ZDS erzeugt und nicht versioniert; die Projekt-/Targetdateien bleiben die
maßgebliche Konfiguration.

## Konfigurationen und Speicher

| Konfiguration | Code | Daten |
| --- | --- | --- |
| Debug | Externes SRAM als Code bei 000000h–07FFFFh | Interner RAM ab B7D000h und externes SRAM bis BFFFFFh |
| Release | Interner Flash | Interner und externer RAM gemäß Release-Linkerdatei |
| Chiponly | Interner Flash | Interner RAM gemäß Chiponly-Linkerdatei |
| DebugTrace | Debug-RAM-Profil mit Trace-Code | Gemäß DebugTrace-Linkerdatei |

Die ausführlich getestete Emulator-Konfiguration ist **Debug**. Andere
Konfigurationen besitzen eigene Linker-/Targetdateien und müssen passend
zur tatsächlichen Hardware geprüft werden.

## Forth verwenden

```forth
1 2 + .
: QUADRAT DUP * ;
12 QUADRAT .
BUTTONS .
1 BUTTON? .
CHAR A MATRIX-CHAR
MATRIX-OFF
```

| Wort | Bedeutung |
| --- | --- |
| `BUTTONS` | Drückmaske 0..7: SW1=1, SW2=2, SW3=4 |
| `BUTTON?` | Nummer 1..3 → -1 bei gedrücktem Taster, sonst 0 |
| `SW1?` `SW2?` `SW3?` | Direkte Tasterabfragen |
| `MATRIX-CHAR` | ASCII 32..126 anzeigen; startet den Timer1-Hintergrundscan |
| `MATRIX!` | Sieben Zeilenbytes eines eigenen Bitmaps kopieren |
| `MATRIX-CLEAR` | Bild löschen, Hintergrundscan weiter betreiben |
| `MATRIX-OFF` | Bild löschen und Timer1 anhalten |

Pro Bitmapzeile sind die unteren fünf Bits aktiv High; Bit 4 ist links.
Die Abfragen liefern den momentanen Tasterzustand ohne Software-Entprellung.
Während der Anzeige gehört Timer1 dem Matrix-Treiber. `BUTTON-INIT` und
`MATRIX-INIT` stehen auch für eine explizite Initialisierung bereit.

## Quellstruktur und Korrekturen

- `asm/CAMLF91e.asm`: primitive Wörter und nativer BSP-/UART-Start;
- `asm/CAMLF91d.asm`: Zellbreite, Codefelder und Compilerhilfen;
- `asm/CAMLF91h.asm`: High-Level-Wörter und Einbindung des Kit-Treibers;
- `asm/kit.inc`, `asm/kit_font.inc`: Taster, Timer1-Matrix und Zeichensatz;
- `asm/cstartup.asm`, `asm/init_params_f91.asm`, `asm/vectors24.asm`: Boardstart;
- `libbsp/`: getrennte Assembler-Bibliothek.

Der 24-Bit-Kern korrigiert `DEPTH`, `FM/MOD`, `M+`, `ACCEPT` und die
Vorzeichenbehandlung von `D.R`/`.R`. Der native Start initialisiert UART0
explizit, bevor die erste Forth-Ausgabe erfolgt.

## Emulator und Tests

Das begleitende [qemuez80f91-Projekt](https://github.com/notwendig/qemuez80f91)
enthält einen reproduzierbaren Polling-Build, 89 Reset-Regressionen, den
vollständigen 237-Wörter-Test mit 201 Ergebnisprüfungen sowie Kit-Tests.
Dort liegen Bedienung, Build-Metadaten und ausführliche Fehleranalysen.
Quellkopien für den Polling-Build werden außerhalb dieses Checkouts erzeugt.

## Ursprung und Nutzungshinweise

Die ursprünglichen Port-/Lizenzhinweise bleiben in [ReadMe.txt](ReadMe.txt),
[LICENSE](LICENSE) und den Quellköpfen erhalten. Insbesondere enthält der
CamelForth-Ursprung gesonderte Hinweise zur persönlichen, pädagogischen und
kommerziellen Nutzung. Dieses README ersetzt diese Hinweise nicht.
