# Changelog

`unicode-collation` uses [PVP Versioning](https://pvp.haskell.org).

## 0.1.3.5

  + Allow text 2.1.

## 0.1.3.4

  + Allow base 4.18.

## 0.1.3.3

  + Allow base 4.17.  Closes #12.

## 0.1.3.2

  * Allow text 2.0.

## 0.1.3.1

  * Allow base 4.16 (so the library can compile with ghc 9.2).

  * Micro-optimization in normalize; update benchmarks.

## 0.1.3

* Add `collateWithUnpacker` (#4).  This allows the library to be
  used with types other than Text.  Alternatively we could use a
  typeclass such as mono-traversable, but this seems a lighter-weight
  solution and keeps dependencies down.

* Add Text.Collate.Normalize, exporting `toNFD`.  By doing our
  own normalization, we avoid a dependency on unicode-transforms,
  and we gain the ability to do normalization incrementally (lazily).
  This is useful because in practice, the ordering of two
  strings is very often decided on the basis of one or two
  initial characters; normalizing the whole string is thus a
  waste of time.

* Improve benchmark suite, with more varied samples.

* Remove dependency on bytestring-lexing; use Data.Text.Read
  instead.

* Add internal module Text.Collate.UnicodeData.
  This generates unicode data from `data/UnicodeData.txt`.
  Remove `data/DerivedCombiningClass.txt`, which is no longer
  needed. to get canonical combining class data.

* Remove dependency on filepath.

* Fix getCollationElements behaviour with discontiguous matches
  (Christian Despres, #5).  The getCollationElements function
  now implements a more or less exact translation of section
  S2.1 of the main UCA algorithm. Since DUCET does not satisfy
  well-formedness condition 5, that function cannot rearrange
  the unblocked non-starters as it was doing previously.  We now
  pass all conformance tests.

* Unit test: skip conformance tests that yield invalid code
  points, as allowed by the spec (#6).  "Implementations that do
  not weight surrogate code points the same way as reserved code
  points may filter out such lines lines in the test cases,
  before testing for conformance." Uncomment the commented-out
  lines in the collation tests.

* Rename internal CombiningClass module -> CanonicalCombiningClass.

* Generalize `matchLongestPrefix` to `Foldable`.
  Rewrite using `foldM` for clarity.

* Rewrite `recursivelyDecompose` using a fold.


## 0.1.2

* API change: Expose `collatorOptions` and `CollatorOptions`.
  Deprecate `collatorLang` which is now redundant.

* API change: Export `renderSortKey`.  This renders the sort key in a compact
  form, used by the CLDR collation tests.  A vertical bar is used in place
  of 0000.

* Remove `optCollation` from `CollatorOptions`.  Make the `Collation`
  a separate parameter of `Collator` instead.  This doesn't affect
  the public API but it makes more sense conceptually.

* Avoid spurious FFFFs in sort keys.  We were including FFFFs at L4
  of sort keys even with NonIgnorable, which is not right, though
  it should not affect the sort.

* Move `VariableWeighting` from `Collation` to `Collator` module.

* Add a benchmark for texts of length 1.

* Small optimization: don't generate sort key when strings are equal.

* Executable: add `--hex` and `--verbose` options.  For testing purposes
  it is convenient to enter code points manually as hex numbers.
  `--verbose` causes diagnostic output to be printed to stderr,
  including the tailoring used, options, and normalized code points
  and sort keys.

## 0.1.1

* API change: Add `collatorLang`, which reports the `Lang` used for
  tailoring (which may be different from the `Lang` passed to
  `collatorFor`, because of fallbacks).

* Fix fallback behavior with `lookupLang` (#3).  Previously `lookupLang`
  would let `de` fall back to `de-u-co-phonebk`.

* Add `--verbose` option to executable. This prints the fallback
  Lang used for tailoring to stderr to help diagnose issues.

## 0.1

* Initial release.

