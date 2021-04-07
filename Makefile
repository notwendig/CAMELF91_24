# Used by eclipse
# link wine's Z: drive to your workspace-root and change the regulary expression below

ZDSII=ZiLOG/ZDSII_eZ80Acclaim\!_5.3.4

BUILD = Debug
ZMAKEFILE = $(wildcard $(BUILD)/*.mak)

all:
	@WINEDEBUG=-all wine $(HOME)/$(ZDSII)/bin/make -C $(BUILD)  -f $(notdir $(ZMAKEFILE)) build \
	| sed -e 'y/\\/\//' -e 's#Z:#/home/juergen/Zilog#g' -e 's/\W\+(\([0-9]\+\)\W\+/:\1:0:/g' -e 's/ERROR/ Fehler:/g' -e 's/WARNING/ Warnung:/g'

clean:
	@WINEDEBUG=-all wine  $(HOME)/$(ZDSII)/bin/make -C $(BUILD)  -f $(notdir $(ZMAKEFILE)) clean \
	| sed -e 'y/\\/\//' -e 's#Z:#/home/juergen/Zilog#g' -e 's/\W\+(\([0-9]\+\),\([0-9]\+\))\W\+/:\1:\2:/g' -e 's/ERROR/ Fehler:/g' -e 's/WARNING/ Warnung:/g'	
