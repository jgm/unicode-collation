test:
	stack test --test-arguments=--hide-successes

bench:
	stack bench

ghci:
	stack ghci unicode-collation:lib unicode-collation:test:unit --ghc-options=-XOverloadedStrings

.PHONY: test bench ghci
