.PHONY: install

install:
	cabal sandbox init
	cabal install --bindir="${HOME}/.cabal/bin"
