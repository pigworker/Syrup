.PHONY: TAGS

TAGS:
	hasktags --etags .

updatetests:
	cabal run syrup:golden-tests -- --accept
