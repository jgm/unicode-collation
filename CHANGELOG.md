# Changelog

`unicode-collation` uses [PVP Versioning](https://pvp.haskell.org).

## 0.1.1

* Fix fallback behavior with `lookupLang` (#3).  Previously `lookupLang`
  would let `de` fall back to `de-u-co-phonebk`.

* Add `collatorLang`, which reports the `Lang` used for
  tailoring (which may be different from the `Lang` passed to
  `collatorFor`, because of fallbacks).

* Add `--verbose` option to executable. This prints the fallback
  Lang used for tailoring to stderr to help diagnose issues.

## 0.1

* Initial release.

