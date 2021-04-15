TAILORINGS=$(patsubst data/perl/%.pl,data/tailorings/%.txt,$(wildcard data/perl/*.pl))

test-stack:
	stack test --test-arguments=--hide-successes && stack runghc test/doctests.hs

test-cabal:
	cabal build --enable-tests --write-ghc-environment-files=always
	cabal test unit --test-option=--hide-successes
	cabal run doctests -fdoctests

bench:
	stack bench --benchmark-arguments="+RTS -T -RTS"

ghci:
	stack ghci --ghc-options=-XOverloadedStrings unicode-collation:lib unicode-collation:test:unit
clean:
	stack clean

tailorings: $(TAILORINGS)

data/tailorings/%.txt: data/perl/%.pl
	awk '/^ENTRY/{exit} f; /ENTRY/{f=1}' $< > $@

.PHONY: test bench ghci clean
