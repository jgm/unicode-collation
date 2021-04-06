{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Copyright: (c) 2021 John MacFarlane
SPDX-License-Identifier: BSD-2-Clause
Maintainer: John MacFarlane <jgm@berkeley.edu>

Haskell implementation of the Unicode Collation Algorithm.

Basic usage example:

>>> import Data.List (sortBy)
>>> :set -XOverloadedStrings
>>> let collator = mkCollator collationOptions
>>> let unsortedList = ["\119990bc","abC","\120146bc","Abc","ab\231","\228bc"]
>>> sortBy (collate collator) unsortedList
["abC","\119990bc","\120146bc","Abc","ab\231","\228bc"]
>>> let se = localizedCollation "se"
>>> let seCollator = mkCollator collationOptions{ optCollation = se }
>>> collate collator "\246" "z"
LT
>>> collate seCollator "\246" "z"
GT
-}

module UnicodeCollation
       ( mkCollator
       , collationOptions
       , localizedCollation
       , rootCollation
       , ducetCollation
       , CollationOptions(..)
       , Collator(..)
       , Collation
       , VariableWeighting(..)
       )
where
import UnicodeCollation.Types
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
import Control.Monad (mplus)
import qualified Data.Binary as Binary

-- | Default 'CollationOptions'.
collationOptions :: CollationOptions
collationOptions =
  CollationOptions
  { optVariableWeighting = NonIgnorable
  , optFrenchAccents     = False
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

-- | Retrieve a collation with language-specific tailorings.
-- If no collations are defined for the language tag, fall back
-- to the bare language tag (without variants or scripts), and
-- then to the root collation.  If no collations are defined
-- with the given collation name, fall back to the default collation
-- for the language.
localizedCollation :: Text  -- ^ BCP 47 language tag, optionally followed by a
                            -- slash and a collation name.  Examples:
                            -- @"fr"@, @"fr-CA"@, @"bs-Cyrl"@,
                            -- @"es/traditional"@, @"zh/stroke"@.
                   -> Collation
localizedCollation spec =
  rootCollation `withTailoring` tailoring
 where
  tailoring = fromMaybe mempty $
    lookupTailoring (lang, mbcollation) `mplus`
    lookupTailoring (baselang, mbcollation) `mplus`
    lookupTailoring (lang, Nothing) `mplus`
    lookupTailoring (lang, Nothing)
  (lang, rest) = T.break (=='/') $ T.map underscoreToHyphen spec
  baselang = T.takeWhile (/='-') lang
  underscoreToHyphen '_' = '-'
  underscoreToHyphen c   = c
  mbcollation = if T.null rest
                   then Nothing
                   else Just $ T.drop 1 rest


-- | Returns a collator constructed using the collation and
-- variable weighting specified in the options.
mkCollator :: CollationOptions -> Collator
mkCollator opts =
  Collator { collate = comparing sortKey'
           , sortKey = sortKey' }
 where
  sortKey' = toSortKey opts (optCollation opts)

toSortKey :: CollationOptions -> Collation -> Text -> SortKey
toSortKey opts collation =
  mkSortKey opts
  . handleVariable (optVariableWeighting opts)
  . getCollationElements collation
  . map ord
  . T.unpack
  . N.normalize N.NFD

handleVariable :: VariableWeighting -> [CollationElement] -> [CollationElement]
handleVariable NonIgnorable = map (\elt -> elt{ collationL4 = 0 })
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
mkSortKey opts elts =
  SortKey (filter (/= 0) $ map collationL1 elts)
          (filter (/= 0) . (if optFrenchAccents opts
                               then reverse
                               else id) $ map collationL2 elts)
          (filter (/= 0) $ map collationL3 elts)
          ((if optVariableWeighting opts == ShiftTrimmed
               then trimTrailingFFFFs
               else id) . filter (/= 0) $ map collationL4 elts)

trimTrailingFFFFs :: [Word16] -> [Word16]
trimTrailingFFFFs = reverse . dropWhile (== 0xFFFF) . reverse

