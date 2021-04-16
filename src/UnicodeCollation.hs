{- |
Copyright: (c) 2021 John MacFarlane
SPDX-License-Identifier: BSD-2-Clause
Maintainer: John MacFarlane <jgm@berkeley.edu>

This library provides a pure Haskell implementation of the
<https://www.unicode.org/reports/tr10 Unicode Collation Algorithm>,
allowing proper sorting of Unicode strings.

The simplest way to use the library is to use the 'IsString'
instance of 'Collator' (together with the @OverloadedStrings@
extension):

>>> import Data.List (sortBy)
>>> import qualified Data.Text.IO as T
>>> mapM_ T.putStrLn $ sortBy (collate "en-US") ["𝒶bc","abC","𝕒bc","Abc","abç","äbc"]
abC
𝒶bc
𝕒bc
Abc
abç
äbc

Note the difference from the default sort:

>>> import Data.List (sort)
>>> import qualified Data.Text.IO as T
>>> mapM_ T.putStrLn $ sort ["𝒶bc","abC","𝕒bc","Abc","abç","äbc"]
Abc
abC
abç
äbc
𝒶bc
𝕒bc

A 'Collator' provides a function 'collate' that compares two texts,
and a function 'sortKey' that returns the sort key.  Most users will
just need 'collate'.

>>> let de = collatorFor "de"
>>> let se = collatorFor "se"
>>> collate de "ö" "z"
LT
>>> collate se "ö" "z"
GT
>>> sortKey de "ö"
SortKey [0x213C,0x2007,0x0000,0x0021,0x0021,0x0000,0x0002,0x0002,0x0000,0xFFFF,0xFFFF]
>>> sortKey se "ö"
SortKey [0x22FD,0x0000,0x0020,0x0000,0x0002,0x0000,0xFFFF]

Because 'Collator' and 'Lang' have 'IsString' instances, you can just specify
them using string literals, as in the above examples.  Note, however,
that you won't get any feedback if the string doesn't parse correctly
as a BCP47 language tag, or if no collation is defined for the specified
language; instead, you'll just get the default (root) collator.  For
this reason, we don't recommend relying on the 'IsString' instance.

If you won't know the language until run time, use 'parseLang'
to parse it to a 'Lang', handling parse errors, and then pass
the 'Lang' to 'collatorFor'.

>>> let handleParseError = error  -- or something fancier
>>> lang <- either handleParseError return $ parseLang "bs-Cyrl"
>>> collate (collatorFor lang) "a" "b"
LT

If you know the language at compile-time, use the 'collator'
quasi-quoter and you'll get compile-time errors and warnings:

>>> :set -XQuasiQuotes
>>> let esTraditional = [collator|es-u-co-trad|]
>>> let esStandard = [collator|es|]
>>> collate esStandard "Co" "Ch"
GT
>>> collate esTraditional "Co" "Ch"
LT

Note that the unicode extension syntax for BCP47 can be used to specify a
particular collation for the language (here, Spanish "traditional" instead of
the default ordering; the alias `trad` is used because of length limits
for BCP47 keywords).

The extension syntax can also be used to set collator options.
The keyword @kb@ can be used to specify the "backwards" accent sorting that is
sometimes used in French:

>>> collate "fr" "côte" "coté"
GT
>>> collate "fr-u-kb" "côte" "coté"
LT

The keyword @ka@ can be used to specify the variable weighting options which
affect how punctuation and whitespace are treated:

>>> collate "en-u-ka-shifted" "de-luge" "de Luge"
LT
>>> collate "en-u-ka-noignore" "de-luge" "de Luge"
GT

The keyword @kk@ can be used to turn off the normalization step (which
is required by the algorithm but can be omitted for better performance
if the input is already in NFD form (canonical decomposition).

>>> let noNormalizeCollator = [collator|en-u-kk-false|]

The keyword @kf@ can be used to say whether uppercase or lowercase
letters should be sorted first.

>>> collate "en-u-kf-upper" "A" "a"
LT
>>> collate "en-u-kf-lower" "A" "a"
GT

These options be combined:

>>> collate "de-DE-u-co-phonebk-kb-false-ka-shifted" "\x00FE" "u"
GT

Options can also be set using the functions 'setVariableWeighting',
'setNormalization', and 'setFrenchAccents':

>>> let frC = setFrenchAccents True [collator|fr|]
>>> collate frC "côte" "coté"
LT

A collation can be "tailored": a tailoring modifies the
collation table in a way that suits a specific locale.

The tailorings provided at <http://unicode.org/Public/cdr/38.1/>
are included in 'tailorings'; to find the one that best matches
a 'Lang', use the function 'lookupLang'.  To apply the
modifications to a collation, just use `<>`.

-}

module UnicodeCollation
       ( Collator
       , collate
       , collatorFor
       , collator
       , rootCollator
       , SortKey(..)
       , sortKey
       , VariableWeighting(..)
       , setVariableWeighting
       , setNormalization
       , setFrenchAccents
       , setUpperBeforeLower
       , tailorings
       , module UnicodeCollation.Lang
       )
where
import UnicodeCollation.Types
import UnicodeCollation.Lang
import UnicodeCollation.Collator
import UnicodeCollation.Tailorings

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XOverloadedStrings


