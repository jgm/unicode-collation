# Changelog

`unicode-collation` uses [PVP Versioning](https://pvp.haskell.org).

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

