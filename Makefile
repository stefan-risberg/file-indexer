all: configure build

configure:
	nix-shell --command "cabal configure"

build:
	cabal build

run:
	cabal run

clean:
	cabal clean
