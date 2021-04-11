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

Locale-specific tailorings are supported, but in a limited
way.  We do not yet support `[reorder..]`.

## Performance

```
  sort a list of 10000 random Texts: OK (2.21s)
    8.2 ms ± 637 μs,  27 MB allocated, 903 KB copied
  sort same list with text-icu:      OK (2.10s)
    2.0 ms ± 114 μs, 7.1 MB allocated, 148 KB copied
```

## Localized collations

The following localized collations are available (if none
are listed for a language, it means that the root collation
will be used):

| Language | Collations |
| -------- | ---------- |
| af | af-u-co-standard |
| am | am-u-co-standard |
| ar | ar-u-co-compat ar-u-co-standard |
| as | as-u-co-standard |
| az | az-u-co-search az-u-co-standard |
| be | be-u-co-standard |
| bg | bg-u-co-standard |
| bn | bn-u-co-standard bn-u-co-trad |
| bo |  |
| br | br-u-co-standard |
| bs | bs-u-co-search bs-u-co-standard bs-Cyrl bs-Cyrl-u-co-standard |
| ca | ca-u-co-search |
| ceb | ceb-u-co-standard |
| chr | chr-u-co-standard |
| cs | cs-u-co-standard |
| cy | cy-u-co-standard |
| da | da-u-co-search da-u-co-standard |
| de | de-u-co-eor de-u-co-phonebk de-u-co-search de-AT de-AT-u-co-phonebk |
| dsb | dsb-u-co-standard |
| dz |  |
| ee | ee-u-co-standard |
| el | el-u-co-standard |
| en | en-US en-US-POSIX en-US-POSIX-u-co-standard |
| eo | eo-u-co-standard |
| es | es-u-co-search es-u-co-standard es-u-co-trad |
| et | et-u-co-standard |
| fa | fa-u-co-standard fa-AF fa-AF-u-co-standard |
| ff | ff-Adlm ff-Adlm-u-co-standard |
| fi | fi-u-co-search fi-u-co-standard fi-u-co-trad |
| fil | fil-u-co-standard |
| fo | fo-u-co-search fo-u-co-standard |
| fr | fr-CA fr-CA-u-co-standard |
| ga |  |
| gl | gl-u-co-search gl-u-co-standard |
| gu | gu-u-co-standard |
| ha | ha-u-co-standard |
| haw | haw-u-co-standard |
| he | he-u-co-search he-u-co-standard |
| hi | hi-u-co-standard |
| hr | hr-u-co-search hr-u-co-standard |
| hsb | hsb-u-co-standard |
| hu | hu-u-co-standard |
| hy | hy-u-co-standard |
| id |  |
| ig | ig-u-co-standard |
| is | is-u-co-search is-u-co-standard |
| it |  |
| ja | ja-u-co-private-kana ja-u-co-standard ja-u-co-unihan |
| ka | ka-u-co-standard |
| kk | kk-u-co-standard |
| kl | kl-u-co-search kl-u-co-standard |
| km | km-u-co-standard |
| kn | kn-u-co-standard kn-u-co-trad |
| ko | ko-u-co-search ko-u-co-searchjl ko-u-co-standard ko-u-co-unihan |
| kok | kok-u-co-standard |
| ku | ku-u-co-standard |
| ky | ky-u-co-standard |
| lb |  |
| lkt | lkt-u-co-standard |
| ln | ln-u-co-phonetic ln-u-co-standard |
| lo | lo-u-co-standard |
| lt | lt-u-co-standard |
| lv | lv-u-co-standard |
| mk | mk-u-co-standard |
| ml | ml-u-co-standard |
| mn | mn-u-co-standard |
| mr | mr-u-co-standard |
| ms |  |
| mt | mt-u-co-standard |
| my | my-u-co-standard |
| nb | nb-u-co-search nb-u-co-standard |
| ne | ne-u-co-standard |
| nl |  |
| nn | nn-u-co-search nn-u-co-standard |
| om | om-u-co-standard |
| or | or-u-co-standard |
| pa | pa-u-co-standard |
| pl | pl-u-co-standard |
| ps | ps-u-co-standard |
| pt |  |
| ro | ro-u-co-standard |
| ru | ru-u-co-standard |
| sa |  |
| se | se-u-co-search se-u-co-standard |
| si | si-u-co-dict si-u-co-standard |
| sk | sk-u-co-search sk-u-co-standard |
| sl | sl-u-co-standard |
| smn | smn-u-co-search smn-u-co-standard |
| sq | sq-u-co-standard |
| sr | sr-u-co-standard sr-Latn sr-Latn-u-co-search sr-Latn-u-co-standard |
| sv | sv-u-co-reformed sv-u-co-search sv-u-co-standard |
| sw |  |
| ta | ta-u-co-standard |
| te | te-u-co-standard |
| th | th-u-co-standard |
| tk | tk-u-co-standard |
| to | to-u-co-standard |
| tr | tr-u-co-search tr-u-co-standard |
| ug | ug-u-co-standard |
| uk | uk-u-co-standard |
| und | und-u-co-emoji und-u-co-eor und-u-co-private-unihan und-u-co-search und-u-co-standard |
| ur | ur-u-co-standard |
| uz | uz-u-co-standard |
| vi | vi-u-co-standard vi-u-co-trad |
| wae |  |
| wo | wo-u-co-standard |
| xh |  |
| yi | yi-u-co-search yi-u-co-standard |
| yo | yo-u-co-standard |
| zh | zh-u-co-big5han zh-u-co-gb2312 zh-u-co-pinyin zh-u-co-private-pinyin zh-u-co-stroke zh-u-co-unihan zh-u-co-zhuyin zh-Hant |
| zu |  |

## Data files

Version 13.0.0 of the Unicode data is used:
<http://www.unicode.org/Public/UCA/13.0.0/>

Locale-specific tailorings are taken from
<http://unicode.org/Public/cdr/38.1/>
(download the zip and extract the collation subdirectory).

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

