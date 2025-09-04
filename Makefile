.PHONY:
	updatetests

all:
	cabal build

updatetests:
	cabal run syrup:golden-tests -- --accept
