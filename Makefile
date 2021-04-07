test:
	stack test --test-arguments=--hide-successes --flag unicode-collation:doctests

bench:
	stack bench --benchmark-arguments="+RTS -T -RTS"

ghci:
	stack ghci unicode-collation:lib unicode-collation:test:unit --ghc-options=-XOverloadedStrings

.PHONY: test bench ghci
