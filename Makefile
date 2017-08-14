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

#alternatively, can use gcc to convert a static library to a shared one (IF the .a was compiled with -fPIC)
#gcc -shared -Wl,--whole-archive libtimetracker.a -o libtimetracker.so

.PHONY: mklibtimetracker-alllibs
mklibtimetracker-alllibs: cmakelibtimetracker
	cd libtimetracker/bin && make -j`nproc`


.PHONY: rename-libs
#cmake can't build static and shared libraries with the same name
rename-libs: mklibtimetracker-alllibs
	cd libtimetracker/bin && \
	    mv libtimetracker_shared.so libtimetracker.so && \
	    mv libtimetracker_static.a  libtimetracker.a

.PHONY: mklibtimetracker
mklibtimetracker: rename-libs

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
