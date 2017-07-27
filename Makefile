LIBTIMETRACKER_ARGS=-DCMAKE_BUILD_TYPE=Debug
HERE=`pwd`

.PHONY: .all
.all: cabalbuild

.PHONY: mklibtimetrackerbin
mklibtimetrackerbin:
	mkdir -p libtimetracker/bin

.PHONY: mklibtimetracker
cmakelibtimetracker: mklibtimetrackerbin
	cd libtimetracker/bin && cmake $(LIBTIMETRACKER_ARGS) .. 

.PHONY: mklibtimetracker
mklibtimetracker: cmakelibtimetracker
	cd libtimetracker/bin && make -j`nproc`

.PHONY: libtimetrackerclean
libtimetrackerclean:
	cd libtimetracker/bin && make clean

.PHONY: cabalconf
cabalconf: mklibtimetracker
	cabal configure --extra-include-dirs=$(HERE)/libtimetracker/include \
	    --extra-lib-dirs=$(HERE)/libtimetracker/bin

.PHONY: cabalbuild
cabalbuild: cabalconf
	cabal build

.PHONY: cabalclean
cabalclean:
	cabal clean

.PHONY: clean
clean: cabalclean libtimetrackerclean
