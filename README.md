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

The library passes all UCA conformance tests.

Localized collations have not been tested as extensively.

## Performance

As might be expected, this library is slower than `text-icu`,
which wraps a heavily optimized C library.  How much slower
depends quite a bit on the input.

On a sample of ten thousand random Unicode strings, we get a
factor of about 3:

```
  sort a list of 10000 random Texts (en):
    5.9 ms ± 487 μs,  22 MB allocated, 899 KB copied
  sort same list with text-icu (en):
    2.1 ms ±  87 μs, 7.1 MB allocated, 148 KB copied
```

Performance is worse on a sample drawn from a smaller character
set including predominantly composed accented letters, which mut
be decomposed as part of the algorithm:

```
  sort a list of 10000 Texts (composed latin) (en):
     12 ms ± 1.1 ms,  34 MB allocated, 910 KB copied
  sort same list with text-icu (en):
    2.3 ms ±  56 μs, 7.0 MB allocated, 146 KB copied
```

Much of the impact here comes from normalization (decomposition).
If we use a pre-normalized sample and disable normalization
in the collator, it's much faster:

```
  sort same list but pre-normalized (en-u-kk-false):
    5.4 ms ± 168 μs,  19 MB allocated, 909 KB copied
```

On plain ASCII, we get a factor of 3 again:

```
  sort a list of 10000 ASCII Texts (en):
    4.6 ms ± 405 μs,  17 MB allocated, 880 KB copied
  sort same list with text-icu (en):
    1.6 ms ± 114 μs, 6.2 MB allocated, 130 KB copied
```

Note that this library does incremental normalization,
so when strings can mostly be distinguished on the basis
of the first two characters, as in the first sample, the
impact is much less.  On the other hand, performance is
much slower on a sample of texts which differ only after
the first 32 characters:

```
  sort a list of 10000 random Texts that agree in first 32 chars:
    116 ms ± 8.6 ms, 430 MB allocated, 710 KB copied
  sort same list with text-icu (en):
    3.2 ms ± 251 μs, 8.8 MB allocated, 222 KB copied
```

However, in the special case where the texts are identical,
the algorithm can be short-circuited entirely and sorting
is very fast:

```
  sort a list of 10000 identical Texts (en):
    877 μs ±  54 μs, 462 KB allocated, 9.7 KB copied
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
To build it, enable the `executable` flag.
For usage instructions, `unicode-collate --help`.

## References

- Unicode Technical Standard #35:
  Unicode Locale Data Markup Language (LDML):
  <http://www.unicode.org/reports/tr35/>
- Unicode Technical Standard #10:
  Unicode Collation Algorithm:
  <https://www.unicode.org/reports/tr10>
- Unicode Technical Standard #215:
  Unicode Normalization Forms:
  <https://unicode.org/reports/tr15/>

