{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Copyright: (c) 2021 John MacFarlane
SPDX-License-Identifier: BSD-2-Clause
Maintainer: John MacFarlane <jgm@berkeley.edu>

Haskell implementation of the
<https://www.unicode.org/reports/tr10 Unicode Collation Algorithm>.

The simplest way to use the library is to create a localized
collator using 'collatorFor', which takes as an argument a
'Lang' representing a BCP47 language tag.

>>> import Data.List (sortBy)
>>> :set -XOverloadedStrings
>>> let unsortedList = ["\119990bc","abC","\120146bc","Abc","ab\231","\228bc"]
>>> let enCollator = collatorFor "en-US"
>>> sortBy (collate enCollator) unsortedList
["abC","\119990bc","\120146bc","Abc","ab\231","\228bc"]
>>> let seCollator = collatorFor "se"
>>> collate enCollator "\246" "z"
LT
>>> collate seCollator "\246" "z"
GT

Because 'Lang' has an 'IsString' instance, you can just specify it
using a string literal, as in the above examples.  Note, however,
that you won't get any feedback if the string doesn't parse correctly
as BCP 47; instead, you'll just get the default (root) collator.

For better safety (and compile-term errors and warnings), use the
quasi-quoter:

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
>>> let frCollatorB = [collator|fr-u-kb|]
>>> collate frCollator "côte" "coté"
GT
>>> collate frCollatorB "côte" "coté"
LT

@ka@ can be used to specify the variable weighting options
which affect how punctuation and whitespace are treated:

>>> let shiftedCollator = [collator|en-u-ka-shifted|]
>>> let nonignorableCollator = [collator|en-u-ka-noignore|]
>>> collate shiftedCollator "de-luge" "de Luge"
LT
>>> collate nonignorableCollator "de-luge" "de Luge"
GT

@kk@ can be used to turn off the normalization step (which
is required by the algorithm but can be omitted for better
performance if the input is already in NFD form (canonical
decomposition).

>>> let noNormalizeCollator = [collator|en-u-kk-false|]

These options be combined:

>>> let complexCollator = [collator|de-DE-u-co-eor-kb-false-ka-shifted-kk|]
>>> collate complexCollator "\x00FE" "u"
LT

If you won't know the language time until run time, use 'parseLang'
to parse it to a 'Lang' rather than using 'fromString', so you can
catch parse errors.

>>> let langtag = "en-US"
>>> :{
let myCollator = case parseLang langtag of
                   Left e     -> error e
                   Right lang -> collatorFor lang
 in collate myCollator "a" "b"
:}
LT

It is also possible to create a collator using the lower-level
interface of 'mkCollator'.
-}

module UnicodeCollation
       ( collatorFor
       , collator
       , mkCollator
       , collationOptions
       , rootCollation
       , ducetCollation
       , tailor
       , tailorings
       , withTailoring
       , CollationOptions(..)
       , Collator(..)
       , Collation
       , VariableWeighting(..)
       , module UnicodeCollation.Lang
       )
where
import UnicodeCollation.Types
import UnicodeCollation.Lang
import UnicodeCollation.Collator
import UnicodeCollation.Tailorings
