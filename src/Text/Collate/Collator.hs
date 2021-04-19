{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Text.Collate.Collator
  ( Collator(..)
  , SortKey(..)
  , renderSortKey
  , VariableWeighting(..)
  , rootCollator
  , collatorLang
  , setVariableWeighting
  , setFrenchAccents
  , setUpperBeforeLower
  , setNormalization
  , collator
  , defaultCollatorOptions
  , collatorFor
  , mkCollator
  )
where

import Text.Collate.Lang
import Text.Collate.Tailorings
import Text.Collate.Collation (getCollationElements, Collation(..),
                               CollationElement(..))
import Data.Word (Word16)
import Data.String
import qualified Data.Text.Normalize as N
import qualified Data.Text as T
import Data.Text (Text)
import Data.Ord (comparing)
import Data.Char (ord)
import Data.List (intercalate)
import Text.Printf (printf)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup (Semigroup(..))
#endif

-- | 'VariableWeighting' affects how punctuation is treated.
-- See <http://www.unicode.org/reports/tr10/#Variable_Weighting>.
data VariableWeighting =
    NonIgnorable   -- ^ Don't ignore punctuation (Deluge < deluge-)
  | Blanked -- ^ Completely ignore punctuation (Deluge = deluge-)
  | Shifted -- ^ Consider punctuation at lower priority
           -- (de-luge < delu-ge < deluge < deluge- < Deluge)
  | ShiftTrimmed -- ^ Variant of Shifted (deluge < de-luge < delu-ge)
  deriving (Show, Eq, Ord)

data CollatorOptions =
  CollatorOptions
  { optLang               :: Maybe Lang -- ^ Which lang was used for tailoring
  , optVariableWeighting  :: VariableWeighting  -- ^ Method for handling
      -- variable elements (see <http://www.unicode.org/reports/tr10/>,
      -- Tables 11 and 12).
  , optFrenchAccents      :: Bool -- ^ If True, secondary weights are scanned
      -- in reverse order, so we get the sorting
      -- "cote côte coté côté" instead of "cote coté côte côté"
  , optUpperBeforeLower   :: Bool -- ^ Sort uppercase letters before lower
  , optNormalize          :: Bool -- ^ If True, strings are normalized
      -- to NFD before collation elements are constructed.  If the input
      -- is already normalized, this option can be set to False for
      -- better performance.
  } deriving (Show, Eq, Ord)

showWordList :: [Word16] -> String
showWordList ws =
    "[" ++ intercalate ","
            (map (printf "0x%04X" . (fromIntegral :: Word16 -> Int)) ws) ++ "]"

newtype SortKey = SortKey [Word16]
  deriving (Eq, Ord)

instance Show SortKey where
 show (SortKey ws) = "SortKey " ++ showWordList ws

-- | Render sort key in the manner used in the CLDR collation test data:
-- the character '|' is used to separate the levels of the key and
-- corresponds to a 0 in the actual sort key.
renderSortKey :: SortKey -> String
renderSortKey (SortKey ws) = "[" ++ tohexes ws ++ "]"
 where
  tohexes = unwords . map tohex
  tohex 0 = "|"
  tohex x = printf "%04X" x

-- Note that & b < q <<< Q is the same as & b < q, & q <<< Q
-- Another syntactic shortcut is:
-- & a <* bcd-gp-s => & a < b < c < d < e < f < g < p < q < r < s
-- & a =* bB => & a = b = B (without that, we have a contraction)
-- &[before 2] a << b => sorts sorts b before a


data Collator = Collator { collate           :: Text -> Text -> Ordering
                         , sortKey           :: Text -> SortKey
                         , collatorOptions   :: CollatorOptions
                         , collatorCollation :: Collation }

instance IsString Collator where
 fromString = collatorFor . fromString

-- | Default collator based on DUCET table (@allkeys.txt@).
rootCollator :: Collator
rootCollator =
  mkCollator defaultCollatorOptions ducetCollation

-- | Report 'Lang' used for tailoring in a collator.
-- Note that because of fallbac rules, this may be somewhat
-- different from the 'Lang' passed to 'collatorFor'.  This 'Lang'
-- won't contain unicode extensions used to set options, but
-- it will contain the collation if a non-default collation is being used.
collatorLang :: Collator -> Maybe Lang
collatorLang = optLang . collatorOptions

modifyCollatorOptions :: (CollatorOptions -> CollatorOptions)
                      -> Collator -> Collator
modifyCollatorOptions f coll =
  mkCollator (f $ collatorOptions coll) (collatorCollation coll)

-- | Set method for handling variable elements (punctuation
-- and spaces): see <http://www.unicode.org/reports/tr10/>,
-- Tables 11 and 12.
setVariableWeighting :: VariableWeighting -> Collator -> Collator
setVariableWeighting w =
  modifyCollatorOptions (\o -> o{ optVariableWeighting = w })

-- | The Unicode Collation Algorithm expects input to be normalized
-- into its canonical decomposition (NFD). By default, collators perform
-- this normalization. If your input is already normalized, you can increase
-- performance by disabling this step: @setNormalization False@.
setNormalization :: Bool -> Collator -> Collator
setNormalization normalize =
  modifyCollatorOptions (\o -> o{ optNormalize = normalize })

-- | @setFrenchAccents True@ causes secondary weights to be scanned
-- in reverse order, so we get the sorting
-- @cote côte coté côté@ instead of @cote coté côte côté@.
-- The default is usually @False@, except for @fr-CA@ where it is @True@.
setFrenchAccents :: Bool -> Collator -> Collator
setFrenchAccents frAccents =
  modifyCollatorOptions (\o -> o{ optFrenchAccents = frAccents })

-- | Most collations default to sorting lowercase letters before
-- uppercase (exceptions: @mt@, @da@, @cu@).  To select the opposite
-- behavior, use @setUpperBeforeLower True@.
setUpperBeforeLower :: Bool -> Collator -> Collator
setUpperBeforeLower upperBefore =
  modifyCollatorOptions (\o -> o{ optUpperBeforeLower = upperBefore })

-- | Create a collator at compile time based on a BCP 47 language
-- tag: e.g., @[collator|es-u-co-trad|]@.  Requires the @QuasiQuotes@ extension.
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
  { optLang              = Nothing
  , optVariableWeighting = NonIgnorable
  , optFrenchAccents     = False
  , optUpperBeforeLower  = False
  , optNormalize         = True
  }

-- | Returns a collator based on a BCP 47 language tag.
-- If no exact match is found, we try to find the best match
-- (falling back to the root collation if nothing else succeeds).
-- If something other than the default collation for a language
-- is desired, the @co@ keyword of the unicode extensions can be
-- used (e.g. @es-u-co-trad@ for traditional Spanish).
-- Other unicode extensions affect the collator options:
--
-- - The @kb@ keyword has the same effect as
--   'setFrenchAccents' (e.g. @fr-FR-u-kb-true@).
-- - The @ka@ keyword has the same effect as 'setVariableWeight'
--   (e.g. @fr-FR-u-kb-ka-shifted@ or @en-u-ka-noignore@).
-- - The @kf@ keyword has the same effect as 'setUpperBeforeLower'
--   (e.g. @fr-u-kf-upper@ or @fr-u-kf-lower@).
-- - The @kk@ keyword has the same effect as 'setNormalization'
--   (e.g. @fr-u-kk-false@).
collatorFor :: Lang -> Collator
collatorFor lang = mkCollator opts collation
  where
    opts = defaultCollatorOptions{
             optLang          = langUsed,
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
                 _               -> True }
    collation = ducetCollation <> tailoring
    (langUsed, tailoring) = case lookupLang lang tailorings of
                              Nothing    -> (Nothing, mempty)
                              Just (l,t) -> (Just l, t)
    exts = langExtensions lang

-- | Returns a collator constructed using the collation and
-- variable weighting specified in the options.
mkCollator :: CollatorOptions -> Collation -> Collator
mkCollator opts collation =
  Collator { collate = \x y -> if x == y  -- optimization
                                  then EQ
                                  else comparing sortKey' x y
           , sortKey = sortKey'
           , collatorOptions = opts
           , collatorCollation = collation
           }
 where
  sortKey' = toSortKey opts collation

toSortKey :: CollatorOptions -> Collation -> Text -> SortKey
toSortKey opts collation =
    mkSortKey opts
  . handleVariable (optVariableWeighting opts)
  . getCollationElements collation
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
    l1s ++ (0:l2s) ++ (0:l3s) ++ if null l4s
                                    then []
                                    else 0:l4s
  where
    l1s = filter (/=0) $ map collationL1 elts
    l2s = (if optFrenchAccents opts
              then reverse
              else id) $ filter (/=0) $ map collationL2 elts
    l3s = filter (/=0) $ map ((if optUpperBeforeLower opts
                                  then switchUpperAndLower
                                  else id) . collationL3) elts
    l4s = case optVariableWeighting opts of
             NonIgnorable -> []
             Blanked      -> []
             ShiftTrimmed -> trimTrailingFFFFs l4s'
             Shifted      -> l4s'
    l4s' = filter (/=0) $ map collationL4 elts
    switchUpperAndLower 0x0002 = 0x0008
    switchUpperAndLower 0x0008 = 0x0002
    switchUpperAndLower x      = x

trimTrailingFFFFs :: [Word16] -> [Word16]
trimTrailingFFFFs = reverse . dropWhile (== 0xFFFF) . reverse

