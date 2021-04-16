{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module UnicodeCollation.Collator
  ( Collator(..)
  , rootCollator
  , setVariableWeighting
  , setFrenchAccents
  , setNormalization
  , collator
  , defaultCollatorOptions
  , collatorFor
  , mkCollator
  )
where

import UnicodeCollation.Types
import UnicodeCollation.Lang
import UnicodeCollation.Tailorings
import UnicodeCollation.Collation (getCollationElements)
import Data.Word (Word16)
import Data.String
import qualified Data.Text.Normalize as N
import qualified Data.Text as T
import Data.Text (Text)
import Data.Ord (comparing)
import Data.Char (ord)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup (Semigroup(..))
#endif

data Collator = Collator { collate         :: Text -> Text -> Ordering
                         , sortKey         :: Text -> SortKey
                         , collatorOptions :: CollatorOptions }

instance IsString Collator where
 fromString = collatorFor . fromString

rootCollator :: Collator
rootCollator =
  mkCollator defaultCollatorOptions{ optCollation = ducetCollation }

setVariableWeighting :: VariableWeighting -> Collator -> Collator
setVariableWeighting w coll =
  mkCollator (collatorOptions coll){ optVariableWeighting = w }

setNormalization :: Bool -> Collator -> Collator
setNormalization normalize coll =
  mkCollator (collatorOptions coll){ optNormalize = normalize }

setFrenchAccents :: Bool -> Collator -> Collator
setFrenchAccents frAccents coll =
  mkCollator (collatorOptions coll){ optFrenchAccents = frAccents }

-- | Create a collator at compile time based on a BCP47 language
-- tag: e.g., @[collator|es-u-co-trad]@.  Requires the @QuasiQuotes@ extension.
collator :: QuasiQuoter
collator = QuasiQuoter
  { quoteExp = \langtag -> do
      case parseLang (T.pack langtag) of
        Left e -> do
          fail $ "Could not parse BCP47 tag " <> langtag <> e
        Right lang ->
          case lookupLang lang tailorings of
            Nothing     -> [| rootCollator |]
            Just (_, _) -> [| collatorFor lang |]
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }


-- | Default 'CollatorOptions'.
defaultCollatorOptions :: CollatorOptions
defaultCollatorOptions =
  CollatorOptions
  { optVariableWeighting = NonIgnorable
  , optFrenchAccents     = False
  , optUpperBeforeLower  = False
  , optNormalize         = True
  , optCollation         = ducetCollation
  }



-- | Returns a collator based on a BCP 47 language tag.
-- If no exact match is found, we try to find the best match
-- (falling back to the root collation if nothing else succeeds).
-- If something other than the default collation for a language
-- is desired, the @co@ keyword of the unicode extensions can be
-- used (e.g. @es-u-co-trad@ for traditional Spanish).
-- The language tag affects not just the collation but the collator
-- options.  The 'optFrenchAccents' option will be set if the
-- unicode extensions (after @-u-@) include the @kb@ keyword
-- (e.g. @fr-FR-u-kb-true@).
-- The 'optVariableWeight' option will be set if the
-- unicode extensions include the @ka@ keyword (e.g. @fr-FR-u-kb-ka-shifted@
-- or @en-u-ka-noignore@).
-- The 'optNormalize' option will be set if the unicode extensions
-- include the @kk@ keyword (e.g. @fr-u-kk-false@).
collatorFor :: Lang -> Collator
collatorFor lang = mkCollator opts
  where
    opts = defaultCollatorOptions{
             optFrenchAccents =
               case lookup "u" exts >>= lookup "kb" of
                 Just ""       -> True
                                       -- true is default attribute value
                 Just "true"   -> True
                 Just _        -> False
                 Nothing       -> langLanguage lang == "cu" ||
                   (langLanguage lang == "fr" && langRegion lang == Just "CA"),
             optVariableWeighting =
               case lookup "u" exts >>= lookup "ka" of
                 Just ""         -> NonIgnorable
                 Just "noignore" -> NonIgnorable
                 Just "shifted"  -> Shifted
                 Nothing | langLanguage lang == "th"
                                 -> Shifted
                 _               -> NonIgnorable,
             optUpperBeforeLower =
               case lookup "u" exts >>= lookup "kf" of
                 Just ""         -> True
                 Just "upper"    -> True
                 Just _          -> False
                 Nothing         -> langLanguage lang == "mt" ||
                                    langLanguage lang == "da" ||
                                    langLanguage lang == "cu",
             optNormalize =
               case lookup "u" exts >>= lookup "kk" of
                 Just ""         -> True
                 Just "true"     -> True
                 Just "false"    -> False
                 _               -> True,
             optCollation = ducetCollation <> tailoring }
    tailoring = maybe mempty snd $ lookupLang lang tailorings
    exts = langExtensions lang

-- | Returns a collator constructed using the collation and
-- variable weighting specified in the options.
mkCollator :: CollatorOptions -> Collator
mkCollator opts =
  Collator { collate = comparing sortKey'
           , sortKey = sortKey'
           , collatorOptions = opts
           }
 where
  sortKey' = toSortKey opts

toSortKey :: CollatorOptions -> Text -> SortKey
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

mkSortKey :: CollatorOptions -> [CollationElement] -> SortKey
mkSortKey opts elts = SortKey $
    l1s ++ (0:l2s) ++ (0:l3s) ++ if null l4s then [] else (0:l4s)
  where
    l1s = filter (/=0) $ map collationL1 elts
    l2s = (if optFrenchAccents opts
              then reverse
              else id) $ filter (/=0) $ map collationL2 elts
    l3s = filter (/=0) $ map ((if optUpperBeforeLower opts
                                  then switchUpperAndLower
                                  else id) . collationL3) elts
    l4s = (case optVariableWeighting opts of
             ShiftTrimmed -> trimTrailingFFFFs
             _             -> id) $ filter (/=0) $ map collationL4 elts
    switchUpperAndLower 0x0002 = 0x0008
    switchUpperAndLower 0x0008 = 0x0002
    switchUpperAndLower x      = x

trimTrailingFFFFs :: [Word16] -> [Word16]
trimTrailingFFFFs = reverse . dropWhile (== 0xFFFF) . reverse

