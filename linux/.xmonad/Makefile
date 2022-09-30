GHC=9.0.2
CABAL=3.8.1.0

.PHONY: build install

build:
	mkdir -p out
	ghcup run --ghc ${GHC} --cabal ${CABAL} -- \
	  cabal install all --installdir=out --overwrite-policy=always
	cp -f $(shell readlink -f out/xmonad-x86_64-linux) .

install: build
	sudo cp -f out/xmonad-dbus /usr/bin
