{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Copyright: (c) 2021 John MacFarlane
SPDX-License-Identifier: BSD-2-Clause
Maintainer: John MacFarlane <jgm@berkeley.edu>

Haskell implementation of the Unicode Collation Algorithm,
described in <https://www.unicode.org/reports/tr10>.

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

-}

module UnicodeCollation
       ( collatorFor
       , collator
       , tailor
       , mkCollator
       , collationOptions
       , rootCollation
       , ducetCollation
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
