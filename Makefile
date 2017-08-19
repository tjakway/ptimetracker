#TODO: DRY!
LIBTIMETRACKER_DEBUG_ARGS=-DCMAKE_BUILD_TYPE=Debug
LIBTIMETRACKER_RELEASE_ARGS=-DCMAKE_BUILD_TYPE=Release

CMAKE_ARGS=$(LIBTIMETRACKER_DEBUG_ARGS)

CABAL_CONF_ARGS=--extra-include-dirs=$(HERE)/libtimetracker/include --extra-lib-dirs=$(HERE)/libtimetracker/bin

CLANG_CXX_DEBUG_FLAGS=-Wformat=2 -Wshadow -ftrapv -Wuninitialized -Winit-self -Wcast-align -Wwrite-strings -fno-omit-frame-pointer

CMAKELISTS_BACKUP=libtimetracker/.cmakelists_backup

HERE=`pwd`


.PHONY: all
all: cabalbuild

.PHONY: mklibtimetrackerbin
mklibtimetrackerbin:
	mkdir -p libtimetracker/bin

.PHONY: cmakelibtimetracker
cmakelibtimetracker: mklibtimetrackerbin
	cd libtimetracker/bin && cmake $(LIBTIMETRACKER_DEBUG_ARGS) -DCMAKE_CXX_FLAGS="$(CLANG_CXX_DEBUG_FLAGS)" .. 

.PHONY: mklibtimetracker
mklibtimetracker: cmakelibtimetracker
	cd libtimetracker/bin && make -j`nproc`

.PHONY: libtimetrackerclean
libtimetrackerclean:
	rm -r -f libtimetracker/bin/*

.PHONY: cabalconf
cabalconf: mklibtimetracker
	cabal configure $(CABAL_CONF_ARGS)

.PHONY: cabalbuild
cabalbuild: cabalconf
	cabal build

.PHONY: cabalclean
cabalclean:
	cabal clean

.PHONY: clean
clean: cabalclean libtimetrackerclean


.PHONY: cmakelibtimetracker-release
cmakelibtimetracker-release: mklibtimetrackerbin
	cd libtimetracker/bin && cmake $(LIBTIMETRACKER_RELEASE_ARGS) ..

.PHONY: cmakelibtimetracker-release
mklibtimetracker-release: cmakelibtimetracker-release
	cd libtimetracker/bin && make -j`nproc` 

.PHONY: cabalconf-release
cabalconf-release: mklibtimetracker-release
	cabal configure $(CABAL_CONF_ARGS) -O2

.PHONY: cabalconf-release
cabalbuild-release: cabalconf-release
	cabal build

.PHONY: release
release: cabalbuild-release

.PHONY: backup-cmakelists
backup-cmakelists:
	cp libtimetracker/CMakeLists.txt $(CMAKELISTS_BACKUP)

.PHONY: restore-cmakelists
restore-cmakelists: 
	mv $(CMAKELISTS_BACKUP) libtimetracker/CMakeLists.txt


#build shared libraries

.PHONY: alter-cmakelists
alter-cmakelists: mklibtimetrackerbin backup-cmakelists
	sed -i -e 's/add_library($${MAIN_TARGET_LIB} STATIC $${PTIMETRACKER_SRCS_NOT_MAIN})/add_library($${MAIN_TARGET_LIB} SHARED $${PTIMETRACKER_SRCS_NOT_MAIN})/g' \
	    libtimetracker/CMakeLists.txt

.PHONY: rm-static-libtimetracker
rm-static-libtimetracker:
	rm -f libtimetracker/bin/libtimetracker.a

.PHONY: cmakelibtimetracker-shared
cmakelibtimetracker-shared: alter-cmakelists rm-static-libtimetracker
	cd libtimetracker/bin && cmake $(CMAKE_ARGS) ..

.PHONY: mklibtimetracker-shared
mklibtimetracker-shared: cmakelibtimetracker-shared
	cd libtimetracker/bin && make -j`nproc`

#now that we're done building libtimetracker, put CMakeLists back how we found it
.PHONY: cabalconf-shared
cabalconf-shared: mklibtimetracker-shared restore-cmakelists
	cabal configure $(CABAL_CONF_ARGS)

.PHONY: cabalbuild-shared
cabalbuild-shared: cabalconf-shared
	cabal build

.PHONY: shared
shared: cabalbuild-shared


#misc commands

.PHONY: repl
repl:
	cabal repl
