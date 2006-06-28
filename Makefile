# arch-tag: Primary makefile
# Copyright (c) 2004-2006 John Goerzen
#

all: setup			# GHC build
	./setup configure
	./setup build

doc: lib/dfs.html/index.html lib/dfs.pdf lib/dfs.ps lib/dfs.txt

hugsbuild: setup
	./setup configure --hugs
	./setup build

setup: Setup.lhs hspod.cabal
	ghc -package Cabal Setup.lhs -o setup

clean:
	-./setup clean
	-cd libsrc && ../setup clean
	-rm -rf dist libsrc/dist *.ho *.hi *.o *.a setup *~
	-cd doc && scons -c && scons -c html pdf text ps
	-rm -rf doc/.sconsign .depend test
	-rm -f `find . -name "*.cm*"` doc/manpage* doc/*.1
	-rm -f `find . -name "*~"` `find . -name "*.o"`


