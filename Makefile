.PHONY:	updatetests TAGS

all:
	cabal build

updatetests:
	cabal run syrup:golden-tests -- --accept

TAGS:
	hasktags --etags .

updatetests:
	cabal run syrup:golden-tests -- --accept
