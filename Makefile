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

data/tailorings/%.txt: Unicode-Collate-1.29/Collate/Locale/%.pl
	awk '/^ENTRY/{exit} f; /ENTRY/{f=1}' $< > $@

Unicode-Collate-1.29/Collate/Locale/%.pl: Unicode-Collate-1.29.tar.gz
	tar xvzf $< $@

Unicode-Collate-1.29.tar.gz:
	wget https://cpan.metacpan.org/authors/id/S/SA/SADAHIRO/Unicode-Collate-1.29.tar.gz

.PHONY: test bench ghci clean
