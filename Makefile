# Build the active ZDS II configuration under Wine.
SHELL := /bin/bash
.SHELLFLAGS := -o pipefail -c
ZDS_HOME ?= $(HOME)/.wine/drive_c/Zilog/ZDSII_eZ80Acclaim!_5.3.5
WINE ?= wine
BUILD ?= Debug
ZMAKEFILE = $(BUILD)/camel_f91_$(BUILD).mak

.PHONY: all clean
all clean:
	@test -f "$(ZDS_HOME)/bin/make.exe" || { echo "Set ZDS_HOME to your ZDS II installation" >&2; exit 1; }
	@test -f "$(ZMAKEFILE)" || { echo "Missing $(ZMAKEFILE); generate it with ZDS II" >&2; exit 1; }
	@cd "$(BUILD)" && WINEDEBUG=-all $(WINE) "$(ZDS_HOME)/bin/make.exe" -f "$(notdir $(ZMAKEFILE))" $(if $(filter clean,$@),clean,build) 2>&1 | sed -e 'y/\\/\//' -e 's#Z:##g'
