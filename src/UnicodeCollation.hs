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

For better safety (and compile-term errors and warnings), use the quasi-quoter:

-- >>> :set -XQuasiQuotation
-- >>> let esTradCollator = [collator|es-u-co-trad|]
-- >>> let esCollator = [collator|es|]
-- >>> let esTradCollator = collatorFor "es-u-co-trad"
-- >>> collate esCollator "Co" "Ch"
-- GT
-- >>> collate esTradCollator "Co" "Ch"
-- LT
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
import UnicodeCollation.TH (genCollation)
import UnicodeCollation.Collation (getCollationElements)
import Data.Word (Word16)
import qualified Data.Text.Normalize as N
import qualified Data.Text as T
import Data.Text (Text)
import Data.Ord (comparing)
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import qualified Data.Binary as Binary

