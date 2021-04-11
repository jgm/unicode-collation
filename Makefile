test-stack:
	stack test --test-arguments=--hide-successes --flag unicode-collation:doctests

test-cabal:
	cabal test -fdoctests --write-ghc-environment-files=always --test-option=--hide-successes

bench:
	stack bench --benchmark-arguments="+RTS -T -RTS"

ghci:
	stack ghci unicode-collation:lib unicode-collation:test:unit --ghc-options=-XOverloadedStrings

clean:
	stack clean

.PHONY: test bench ghci clean
