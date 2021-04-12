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

>>> import Data.List (sort, sortBy)
>>> :set -XOverloadedStrings
>>> let unsortedList = ["\119990bc","abC","\120146bc","Abc","ab\231","\228bc"]
>>> let enCollator = collatorFor "en-US"
>>> sortBy (collate enCollator) unsortedList
["abC","\119990bc","\120146bc","Abc","ab\231","\228bc"]
>>> sort unsortedList  -- note the difference from the default sort:
["Abc","abC","ab\231","\228bc","\119990bc","\120146bc"]

A 'Collator' provides a function 'collate' that compares two texts,
and a function 'sortKey' that returns the sort key.

>>> let deCollator = collatorFor "de"
>>> let seCollator = collatorFor "se"
>>> collate deCollator "\246" "z"
LT
>>> collate seCollator "\246" "z"
GT
>>> sortKey deCollator "z"
SortKey [0x2286,0x0000,0x0020,0x0000,0x0002,0x0000,0xFFFF]

Because 'Lang' has an 'IsString' instance, you can just specify it
using a string literal, as in the above examples.  Note, however,
that you won't get any feedback if the string doesn't parse correctly
as BCP 47, or if no collation is defined for the specified language;
instead, you'll just get the default (root) collator.

If you won't know the language until run time, use 'parseLang'
to parse it to a 'Lang' rather than using 'fromString', so you can
catch parse errors.

If you know the language at compile-time, use the quasi-quoter
and you'll get compile-time errors and warnings:

>>> :set -XQuasiQuotes
>>> let esTradCollator = [collator|es-u-co-trad|]
>>> let esCollator = [collator|es|]
>>> collate esCollator "Co" "Ch"
GT
>>> collate esTradCollator "Co" "Ch"
LT

Note that the unicode extension syntax for BCP47 can be used to specify
a particular collation (here, Spanish "traditional" instead of the
default ordering).

The extension syntax can also be used to set other collator options:
@kb@ can be used to specify the "backwards" accent sorting that
is sometimes used in French:

>>> let frCollator = [collator|fr|]
>>> collate frCollator "côte" "coté"
GT
>>> let frCollatorB = [collator|fr-u-kb|]
>>> collate frCollatorB "côte" "coté"
LT

@ka@ can be used to specify the variable weighting options
which affect how punctuation and whitespace are treated:

>>> let shiftedCollator = [collator|en-u-ka-shifted|]
>>> collate shiftedCollator "de-luge" "de Luge"
LT
>>> let nonignorableCollator = [collator|en-u-ka-noignore|]
>>> collate nonignorableCollator "de-luge" "de Luge"
GT

@kk@ can be used to turn off the normalization step (which
is required by the algorithm but can be omitted for better
performance if the input is already in NFD form (canonical
decomposition).

>>> let noNormalizeCollator = [collator|en-u-kk-false|]

These options be combined:

>>> let complexCollator = [collator|de-DE-u-co-eor-kb-false-ka-shifted|]
>>> collate complexCollator "\x00FE" "u"
LT

Options can also be set using the functions 'setVariableWeighting',
'setNormalization', and 'setFrenchAccents':

>>> let frCollatorC = setFrenchAccents True [collator|fr|]
>>> collate frCollatorB "côte" "coté"
LT

A collation can be "tailored": a tailoring modifies the
collation table in a way that suits a specific locale.

The tailorings provided at <http://unicode.org/Public/cdr/38.1/>
are included in 'tailorings'; to find the one that best matches
a 'Lang', use the function 'lookupLang'.  But you can also create
custom tailorings using the syntax described in
<http://www.unicode.org/reports/tr35/>, using the 'tailor'
quasi-quoter:

>>> :set -XQuasiQuotes
>>> let crazyCollator = rootCollator `withTailoring` [tailor|&ex < d|]
>>> collate crazyCollator "direct" "extract"
GT
>>> collate crazyCollator "figure" "extract"
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

