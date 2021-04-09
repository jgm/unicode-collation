{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Copyright: (c) 2021 John MacFarlane
SPDX-License-Identifier: BSD-2-Clause
Maintainer: John MacFarlane <jgm@berkeley.edu>

Haskell implementation of the Unicode Collation Algorithm,
described in <https://www.unicode.org/reports/tr10>.

Basic usage example:

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
>>> let esCollator = collatorFor "es"
>>> let esTradCollator = collatorFor "es-u-co-trad"
>>> collate esCollator "Co" "Ch"
GT
>>> collate esTradCollator "Co" "Ch"
LT
-}

module UnicodeCollation
       ( collatorFor
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
import UnicodeCollation.Tailorings
import UnicodeCollation.TH (genCollation)
import UnicodeCollation.Elements (getCollationElements)
import Data.Word (Word16)
import qualified Data.Text.Normalize as N
import qualified Data.Text as T
import Data.Text (Text)
import Data.Ord (comparing)
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import qualified Data.Binary as Binary

-- | Default 'CollationOptions'.
collationOptions :: CollationOptions
collationOptions =
  CollationOptions
  { optVariableWeighting = NonIgnorable
  , optFrenchAccents     = False
  , optNormalize         = True
  , optCollation         = rootCollation
  }

-- | The root collation defined in allkeys_CLDR.txt is the collation
-- on which all the other localized tailorings are based.  Use this
-- if you want to tailor your collation.
rootCollation :: Collation
rootCollation = Binary.decode $(genCollation "data/allkeys_CLDR.txt")

-- | The DUCET collation defined in allkeys.txt is used in the
-- conformance tests.
ducetCollation :: Collation
ducetCollation = Binary.decode $(genCollation "data/allkeys.txt")

-- | Returns a collator based on a BCP 47 language tag.
-- If no exact match is found, we try the following fallbacks
-- in order: (1) remove extensions, (2) remove variants,
-- (3) remove region, (4) remove script, (5) fall back to
-- root collation.
collatorFor :: Lang -> Collator
collatorFor lang = mkCollator opts
  where
    opts = collationOptions{
             optFrenchAccents =
               case lookup "u" (langExtensions lang) >>= lookup "kb" of
                 Just Nothing       -> True
                                       -- true is default attribute value
                 Just (Just "true") -> True
                 _                  -> False,
             optVariableWeighting =
               case lookup "u" (langExtensions lang) >>= lookup "ka" of
                 Just Nothing           -> NonIgnorable
                 Just (Just "noignore") -> NonIgnorable
                 Just (Just "shifted")  -> Shifted
                 _                      -> NonIgnorable,
             optCollation = rootCollation `withTailoring` tailoring }
    tailoring = fromMaybe mempty $ lookupLang lang tailorings

-- | Returns a collator constructed using the collation and
-- variable weighting specified in the options.
mkCollator :: CollationOptions -> Collator
mkCollator opts =
  Collator { collate = comparing sortKey'
           , sortKey = sortKey' }
 where
  sortKey' = toSortKey opts

toSortKey :: CollationOptions -> Text -> SortKey
toSortKey opts =
    mkSortKey opts
  . handleVariable (optVariableWeighting opts)
  . getCollationElements (optCollation opts)
  . T.foldr ((:) . ord) []
  . if optNormalize opts
       then N.normalize N.NFD
       else id

handleVariable :: VariableWeighting -> [CollationElement] -> [CollationElement]
handleVariable NonIgnorable = id
handleVariable Blanked = doVariable False False
handleVariable Shifted = doVariable True False
handleVariable ShiftTrimmed = handleVariable Shifted

doVariable :: Bool -> Bool -> [CollationElement] -> [CollationElement]
doVariable _useL4 _afterVariable [] = []
doVariable useL4 afterVariable (e:es)
  | collationVariable e
    =   e{ collationL1 = 0, collationL2 = 0, collationL3 = 0,
           collationL4 = -- Table 11
             case useL4 of
               True
                 | collationL1 e == 0
                 , collationL2 e == 0
                 , collationL3 e == 0   -> 0
                 | collationL1 e == 0
                 , collationL3 e /= 0
                 , afterVariable        -> 0
                 | collationL1 e /= 0   -> collationL1 e
                 | collationL1 e == 0
                 , collationL3 e /= 0
                 , not afterVariable    -> 0xFFFF
               _                        -> 0
         } : doVariable useL4 True es
  | collationL1 e == 0 -- "ignorable"
  , afterVariable
    = e{ collationL1 = 0, collationL2 = 0, collationL3 = 0, collationL4 = 0 }
       : doVariable useL4 afterVariable es
  | collationL1 e /= 0
  , not (collationVariable e)
  , useL4
  = e{ collationL4 = 0xFFFF } : doVariable useL4 False es
  | otherwise
    = e : doVariable useL4 False es

mkSortKey :: CollationOptions -> [CollationElement] -> SortKey
mkSortKey opts elts = SortKey $
    l1s ++ (0:l2s) ++ (0:l3s) ++ if null l4s then [] else (0:l4s)
  where
    l1s = filter (/=0) $ map collationL1 elts
    l2s = (if optFrenchAccents opts
              then reverse
              else id) $ filter (/=0) $ map collationL2 elts
    l3s = filter (/=0) $ map collationL3 elts
    l4s = (case optVariableWeighting opts of
             ShiftTrimmed -> trimTrailingFFFFs
             _             -> id) $ filter (/=0) $ map collationL4 elts

trimTrailingFFFFs :: [Word16] -> [Word16]
trimTrailingFFFFs = reverse . dropWhile (== 0xFFFF) . reverse

