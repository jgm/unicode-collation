# unicode-collation

[![GitHub
CI](https://github.com/jgm/unicode-collation/workflows/CI%20tests/badge.svg)](https://github.com/jgm/unicode-collation/actions)
[![Hackage](https://img.shields.io/hackage/v/unicode-collation.svg?logo=haskell)](https://hackage.haskell.org/package/unicode-collation)
[![BSD-2-Clause license](https://img.shields.io/badge/license-BSD--2--Clause-blue.svg)](LICENSE)

Haskell implementation of [unicode collation algorithm].

[unicode collation algorithm]:  https://www.unicode.org/reports/tr10

## Motivation

Previously there was no way to do correct unicode collation
(sorting) in Haskell without depending on the C library `icu`
and the barely maintained Haskell wrapper `text-icu`.  This
library offers a pure Haskell solution.

## Conformance

The library passes UCA conformance tests (except for tests
involving unmatched surrogates and a few Tibetan characters,
which seem to be changed in unexpected ways by Text.pack or
normalization).

Localized collations have not been tested extensively.

## Performance

```
  sort a list of 10000 random Texts: OK (2.21s)
    8.2 ms ± 637 μs,  27 MB allocated, 903 KB copied
  sort same list with text-icu:      OK (2.10s)
    2.0 ms ± 114 μs, 7.1 MB allocated, 148 KB copied
```

## Localized collations

The following localized collations are available.
For languages not listed here, the root collation is
used.

```
af
ar
as
az
be
bn
ca
cs
cu
cy
da
de-AT-u-co-phonebk
de-u-co-phonebk
dsb
ee
eo
es
es-u-co-trad
et
fa
fi
fi-u-co-phonebk
fil
fo
fr-CA
gu
ha
haw
he
hi
hr
hu
hy
ig
is
ja
kk
kl
kn
ko
kok
lkt
ln
lt
lv
mk
ml
mr
mt
nb
nn
nso
om
or
pa
pl
ro
sa
se
si
si-u-co-dict
sk
sl
sq
sr
sv
sv-u-co-reformed
ta
te
th
tn
to
tr
ug-Cyrl
uk
ur
vi
vo
wae
wo
yo
zh
zh-u-co-big5han
zh-u-co-gb2312
zh-u-co-pinyin
zh-u-co-stroke
zh-u-co-zhuyin
```

Collation reordering (e.g. `[reorder Latn Kana Hani]`)
is not suported

## Data files

Version 13.0.0 of the Unicode data is used:
<http://www.unicode.org/Public/UCA/13.0.0/>

Locale-specific tailorings are derived from the Perl
module Unicode::Collate:
https://cpan.metacpan.org/authors/id/S/SA/SADAHIRO/Unicode-Collate-1.29.tar.gz

## Executable

The package includes an executable component, `unicode-collate`,
which may be used for testing and for collating in scripts.
For usage instructions, `unicode-collate --help`.

## References

- Unicode Technical Standard #35:
  Unicode Locale Data Markup Language (LDML):
  <http://www.unicode.org/reports/tr35/>
- Unicode Technical Standard #10:
  Unicode Collation Algorithm:
  <https://www.unicode.org/reports/tr10>

