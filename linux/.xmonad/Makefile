GHC=9.0.2
CABAL=3.8.1.0

.PHONY: install

xmonad-x86_64-linux: cabal.project.freeze
	mkdir -p out
	ghcup run --install --ghc ${GHC} --cabal ${CABAL} -- \
	  cabal install all --installdir=out --overwrite-policy=always
	cp -f $(shell readlink -f out/xmonad-x86_64-linux) .

cabal.project.freeze:
	ghcup run --install --ghc ${GHC} --cabal ${CABAL} -- \
	  cabal freeze

install: xmonad-x86_64-linux
	sudo cp -f out/xmonad-dbus /usr/bin
