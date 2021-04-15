test-stack:
	stack test --test-arguments=--hide-successes --flag unicode-collation:doctests

test-cabal:
	cabal build --enable-tests --write-ghc-environment-files=always
	cabal test unit --test-option=--hide-successes
	cabal test -fdoctests doctests

bench:
	stack bench --benchmark-arguments="+RTS -T -RTS"

ghci:
	stack ghci --ghc-options=-XOverloadedStrings unicode-collation:lib unicode-collation:test:unit
clean:
	stack clean

.PHONY: test bench ghci clean
