{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Copyright: (c) 2021 John MacFarlane
SPDX-License-Identifier: BSD-2-Clause
Maintainer: John MacFarlane <jgm@berkeley.edu>

This library provides a pure Haskell implementation of the
<https://www.unicode.org/reports/tr10 Unicode Collation Algorithm>,
allowing proper sorting of Unicode strings.

The simplest way to use the library is to create a localized
collator using 'collatorFor', which takes as an argument a
'Lang' representing a BCP47 language tag.

>>> import Data.List (sortBy)
>>> import qualified Data.Text.IO as T
>>> let en_US = collatorFor "en-US"
>>> mapM_ T.putStrLn $ sortBy (collate en_US) ["𝒶bc","abC","𝕒bc","Abc","abç","äbc"]
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
SortKey [0x213C,0x0000,0x0020,0x002B,0x0000,0x0002,0x0002,0x0000,0xFFFF,0xFFFF]
>>> sortKey se "ö"
SortKey [0x2302,0x0000,0x0022,0x0000,0x0009,0x0000,0xFFFF]

Because 'Lang' has an 'IsString' instance, you can just specify it
using a string literal, as in the above examples.  Note, however,
that you won't get any feedback if the string doesn't parse correctly
as BCP 47, or if no collation is defined for the specified language;
instead, you'll just get the default (root) collator.  For this
reason, we don't recommend relying on the 'IsString' instance.

If you won't know the language until run time, use 'parseLang'
to parse it to a 'Lang', handling parse errors, and then pass
the 'Lang' to 'collatorFor'.

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

>>> let fr = [collator|fr|]
>>> collate fr "côte" "coté"
GT
>>> let frB = [collator|fr-u-kb|]
>>> collate frB "côte" "coté"
LT

The keyword @ka@ can be used to specify the variable weighting options which
affect how punctuation and whitespace are treated:

>>> let shifted = [collator|en-u-ka-shifted|]
>>> collate shifted "de-luge" "de Luge"
LT
>>> let nonignorable = [collator|en-u-ka-noignore|]
>>> collate nonignorable "de-luge" "de Luge"
GT

The keyword @kk@ can be used to turn off the normalization step (which
is required by the algorithm but can be omitted for better performance
if the input is already in NFD form (canonical decomposition).

>>> let noNormalizeCollator = [collator|en-u-kk-false|]

These options be combined:

>>> let complexCollator = [collator|de-DE-u-co-eor-kb-false-ka-shifted|]
>>> collate complexCollator "\x00FE" "u"
LT

Options can also be set using the functions 'setVariableWeighting',
'setNormalization', and 'setFrenchAccents':

>>> let frC = setFrenchAccents True [collator|fr|]
>>> collate frC "côte" "coté"
LT

A collation can be "tailored": a tailoring modifies the
collation table in a way that suits a specific locale.

The tailorings provided at <http://unicode.org/Public/cdr/38.1/>
are included in 'tailorings'; to find the one that best matches
a 'Lang', use the function 'lookupLang'.  But you can also create
custom tailorings using the syntax described in
<http://www.unicode.org/reports/tr35/>, using the 'tailor'
quasi-quoter:

>>> let crazy = rootCollator `withTailoring` [tailor|&ex < d|]
>>> collate crazy "direct" "extract"
GT
>>> collate crazy "figure" "extract"
GT

-}

module UnicodeCollation
       ( Collator
       , collate
       , collatorFor
       , collator
       , rootCollator
       , ducetCollator
       , SortKey(..)
       , sortKey
       , VariableWeighting(..)
       , setVariableWeighting
       , setNormalization
       , setFrenchAccents
       , Tailoring
       , withTailoring
       , tailor
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

rootCollator :: Collator
rootCollator = mkCollator collationOptions{ optCollation = rootCollation }

ducetCollator :: Collator
ducetCollator = mkCollator collationOptions{ optCollation = ducetCollation }

setVariableWeighting :: VariableWeighting -> Collator -> Collator
setVariableWeighting w coll =
  mkCollator (collatorOptions coll){ optVariableWeighting = w }

setNormalization :: Bool -> Collator -> Collator
setNormalization normalize coll =
  mkCollator (collatorOptions coll){ optNormalize = normalize }

setFrenchAccents :: Bool -> Collator -> Collator
setFrenchAccents frAccents coll =
  mkCollator (collatorOptions coll){ optFrenchAccents = frAccents }

