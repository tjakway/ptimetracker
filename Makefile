LIBTIMETRACKER_ARGS=-DCMAKE_BUILD_TYPE=Debug
HERE=`pwd`
EXE_NAME=ptimetracker
CABAL_CONF_ARGS=--extra-include-dirs=$(HERE)/libtimetracker/include --extra-lib-dirs=$(HERE)/libtimetracker/bin
CABAL_STATIC_ARGS=--disable-executable-dynamic --ghc-option=-optl=-static

.PHONY: all
all: cabalbuild

.PHONY: static
static: cabalbuild-static

.PHONY: mklibtimetrackerbin
mklibtimetrackerbin:
	mkdir -p libtimetracker/bin

.PHONY: cmakelibtimetracker
cmakelibtimetracker: mklibtimetrackerbin
	cd libtimetracker/bin && cmake $(LIBTIMETRACKER_ARGS) .. 

.PHONY: mklibtimetracker-staticlib
mklibtimetracker-staticlib: cmakelibtimetracker
	cd libtimetracker/bin && make -j`nproc`

.PHONY: mksharedlib
mksharedlib: mklibtimetracker-staticlib
	cd libtimetracker/bin && gcc -shared -Wl,--whole-archive,-soname libtimetracker.a -o libtimetracker.so

.PHONY: mklibtimetracker
mklibtimetracker: mksharedlib

.PHONY: libtimetrackerclean
libtimetrackerclean:
	rm -r -f libtimetracker/bin/*

.PHONY: cabalconf
cabalconf: mklibtimetracker
	cabal configure $(CABAL_CONF_ARGS)

.PHONY: cabalconf-static
cabalconf-static: mklibtimetracker
	cabal configure $(CABAL_CONF_ARGS) $(CABAL_STATIC_ARGS)	

.PHONY: cabalbuild-static
cabalbuild-static: cabalconf-static
	cabal build

.PHONY: cabalbuild
cabalbuild: cabalconf
	cabal build

.PHONY: cabalclean
cabalclean:
	cabal clean

.PHONY: clean
clean: cabalclean libtimetrackerclean
