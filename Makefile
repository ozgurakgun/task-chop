.PHONY: install

install:
	cabal sandbox init
	cabal install --only-dependencies
	cabal install -j1 --bindir="${HOME}/.cabal/bin"
