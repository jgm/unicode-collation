{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveLift #-}
module UnicodeCollation.Types
  ( Collator(..)
  , CollationOptions(..)
  , VariableWeighting(..)
  , CollationElement(..)
  , Collation(..)
  , SortKey(..)
  , Tailoring(..)
  , CollationMod(..)
  , Target(..)
  , Category(..)
  , Level(..)
  , insertElements
  , alterElements
  , findLast
  , findFirst
  , hasCategory
  , matchLongestPrefix
  )
where

import Data.Word (Word16, Word8)
import Data.Binary (Binary(get, put))
import Data.Foldable (minimumBy, maximumBy)
import Data.Text (Text)
import Language.Haskell.TH.Syntax (Lift(..))
import Instances.TH.Lift ()
import qualified UnicodeCollation.Trie as Trie
import Text.Printf
import Data.List (intercalate)
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup (Semigroup(..))
#endif

data CollationOptions =
  CollationOptions
  { optVariableWeighting  :: VariableWeighting  -- ^ Method for handling
      -- variable elements (see <http://www.unicode.org/reports/tr10/>,
      -- Tables 11 and 12).
  , optFrenchAccents      :: Bool -- ^ If True, secondary weights are scanned
      -- in reverse order, so we get the sorting
      -- "cote côte coté côté" instead of "cote coté côte côté"
  , optNormalize          :: Bool -- ^ If True, strings are normalized
      -- to NFD before collation elements are constructed.  If the input
      -- is already normalized, this option can be set to False for
      -- better performance.
  , optCollation          :: Collation  -- ^ The collation to use.
  } deriving (Show, Eq, Ord)

-- | 'VariableWeighting' affects how punctuation is treated.
-- See <http://www.unicode.org/reports/tr10/#Variable_Weighting>.
data VariableWeighting =
    NonIgnorable   -- ^ Don't ignore punctuation (Deluge < deluge-)
  | Blanked -- ^ Completely ignore punctuation (Deluge = deluge-)
  | Shifted -- ^ Consider punctuation at lower priority
           -- (de-luge < delu-ge < deluge < deluge- < Deluge)
  | ShiftTrimmed -- ^ Variant of Shifted (deluge < de-luge < delu-ge)
  deriving (Show, Eq, Ord)

data Collator = Collator { collate :: Text -> Text -> Ordering
                         , sortKey :: Text -> SortKey }

data CollationElement =
  CollationElement
    { collationVariable :: !Bool
    , collationL1       :: {-# UNPACK #-} !Word16
    , collationL2       :: {-# UNPACK #-} !Word16
    , collationL3       :: {-# UNPACK #-} !Word16
    , collationL4       :: {-# UNPACK #-} !Word16
    } deriving (Eq, Lift)

instance Ord CollationElement where
 compare (CollationElement _ p1 s1 t1 q1) (CollationElement _ p2 s2 t2 q2) =
   compare p1 p2 <> compare s1 s2 <> compare t1 t2 <> compare q1 q2

instance Show CollationElement where
  show (CollationElement v l1 l2 l3 l4) =
    printf "CollationElement %s 0x%04X 0x%04X 0x%04X 0x%04X" (show v) l1 l2 l3 l4

instance Binary CollationElement where
   put (CollationElement v w x y z) = put (v,w,x,y,z)
   get = do
     (v,w,x,y,z) <- get
     return $ CollationElement v w x y z

newtype Collation = Collation { unCollation :: Trie.Trie [CollationElement] }
  deriving (Show, Eq, Ord, Lift, Semigroup, Monoid)

-- | Insert collation elements for the given code points (if tehre is
-- more than one code point, it is a contraction).
insertElements :: [Int] -> [CollationElement] -> Collation -> Collation
insertElements codepoints els (Collation trie) =
  Collation $ Trie.insert codepoints els trie

-- | Change the collation elements defined for the specified code point(s).
alterElements :: (Maybe [CollationElement] -> Maybe [CollationElement])
              -> [Int] -> Collation -> Collation
alterElements f codepoints (Collation trie) =
  Collation $ Trie.alter f codepoints trie

-- | Find the longest matching prefix of a list of code points
-- in the collation table. This may be a single code point or
-- several (if contractions are defined).  Return the
-- collation elements for the matched code points, the code points
-- matched, and a "subcollation" which can be searched for further
-- matches. (This is needed because of "discontiguous matches";
-- see <http://www.unicode.org/reports/tr10/#Input_Matching>.)
matchLongestPrefix :: Collation
                   -> [Int]
                   -> Maybe ([CollationElement], [Int], Collation)
matchLongestPrefix (Collation trie) codepoints =
  case Trie.matchLongestPrefix trie codepoints of
    Nothing -> Nothing
    Just (els, is, trie') -> Just (els, is, Collation trie')

-- | Find the first element in a 'Collation' that meets a condition.
-- Return the code points and the elements.
findFirst :: ([CollationElement] -> Bool)
          -> Collation
          -> Maybe ([Int], [CollationElement])
findFirst f (Collation trie) =
  case minimumBy comp $ Trie.unfoldTrie trie of
    (is,elts) | f elts -> Just (is,elts)
    _ -> Nothing
 where
  comp (_,x) (_,y) =  -- note Left a < Right a
    compare (if f x then Left x else Right x)
            (if f y then Left y else Right y)

-- | Find the last element in a 'Collation' that meets a condition.
-- Return the code points and the elements.
findLast :: ([CollationElement] -> Bool)
         -> Collation
         -> Maybe ([Int], [CollationElement])
findLast f (Collation trie) =
  case maximumBy comp $ Trie.unfoldTrie trie of
    (is,elts) | f elts -> Just (is,elts)
    _ -> Nothing
 where
  comp (_,x) (_,y) =  -- note Left a < Right a
    compare (if f x then Right x else Left x)
            (if f y then Right y else Left y)

-- | Test a list of collation elements to see if they belong
-- to the specified 'Category'.
hasCategory :: [CollationElement] -> Category -> Bool
hasCategory [] TertiaryIgnorable = True
hasCategory [] SecondaryIgnorable = True
hasCategory [] PrimaryIgnorable = True
hasCategory [] _ = False
hasCategory (CollationElement v p s t _:_) cat =
  case cat of
    TertiaryIgnorable  -> p == 0 && s== 0 && t == 0
    SecondaryIgnorable -> p == 0 && s == 0
    PrimaryIgnorable   -> p == 0
    Variable           -> v
                           -- docs say: if alternate = non-ignorable
                           --                 p != ignore
                           --           if alternate = shifted
                           --                 p,s,t = ignore
    Regular            -> not v && p /= 0
                           -- docs say: [last regular] is not actually the last
                           -- normal CE with a primary weight ... [last regular]
                           -- is set to the first Hani CE, the artificial
                           -- script boundary CE at the beginning of this range.
                           -- We handle this specially in Mods.
    Implicit           -> p /= 0
                           --  this is meant for items that are given
                           --  values implicitly, not in table. Handle in Mods.
    Trailing           -> p /= 0  -- TODO ??
                           -- "used for trailing syllable components"


instance Binary Collation where
   put (Collation m) = put m
   get = Collation <$> get

showWordList :: [Word16] -> String
showWordList ws =
    "[" ++ intercalate ","
            (map (printf "0x%04X" . (fromIntegral :: Word16 -> Int)) ws) ++ "]"

newtype SortKey = SortKey { unSortKey :: [Word16] }
  deriving (Eq, Ord)

instance Show SortKey where
 show (SortKey ws) = "SortKey " ++ showWordList ws

-- Note that & b < q <<< Q is teh same as & b < q, & q <<< Q
-- Another syntactic shortcut is:
-- & a <* bcd-gp-s => & a < b < c < d < e < f < g < p < q < r < s
-- & a =* bB => & a = b = B (without that, we have a contraction)
-- &[before 2] a << b => sorts sorts b before a

data Level = L1 | L2 | L3 | L4
  deriving (Show, Eq, Ord, Enum, Lift)

instance Binary Level where
   put = put . fromEnum
   get = toEnum <$> get

-- | A 'Target' for a 'CollationMod' may be some Text
-- (a code point or contraction) or it may be specified as
-- the first or last element in a given category, e.g. in
-- @&[first regular] < b@. See
-- <https://unicode.org/reports/tr35/tr35-collation.html#Logical_Reset_Positions>.
data Target =
    TargetText Text
  | TargetFirst Category
  | TargetLast Category
  deriving (Show, Eq, Ord, Lift)

instance Binary Target where
   put (TargetText t) = put (0x12 :: Word8, t)
   put (TargetFirst cat) = put (0x14 :: Word8, cat)
   put (TargetLast cat) = put (0x16 :: Word8, cat)
   get = do
     (x :: Word8) <- get
     case x of
       0x12 -> TargetText <$> get
       0x14 -> TargetFirst <$> get
       0x16 -> TargetLast <$> get
       _    -> fail "Unknown code for Target"

-- | See <https://unicode.org/reports/tr35/tr35-collation.html#Logical_Reset_Positions>.
data Category =
    TertiaryIgnorable
  | SecondaryIgnorable
  | PrimaryIgnorable
  | Variable
  | Regular
  | Implicit
  | Trailing
  deriving (Show, Eq, Ord, Lift, Enum)

instance Binary Category where
   put = put . fromEnum
   get = toEnum <$> get

-- | A single modification of a 'Collation', putting one code point
-- or contraction before or after or equal to another.
data CollationMod =
    After Level Target Target     -- 'a < b' (b after a primary)
  | Before Level Target Target    -- '&[before 1] a < b' (b before a primary)
  | Equal Target Target           -- 'a = b' (sort identically)
  deriving (Show, Eq, Ord, Lift)

instance Binary CollationMod where
  put (After l e1 e2) = put (0x21 :: Word8, l, e1, e2)
  put (Before l e1 e2) = put (0x22 :: Word8, l, e1, e2)
  put (Equal e1 e2) = put (0x23 :: Word8, e1, e2)
  get = do
    (x :: Word8) <- get
    case x of
      0x21 -> After <$> get <*> get <*> get
      0x22 -> Before <$> get <*> get <*> get
      0x23 -> Equal <$> get <*> get
      _ -> fail "Uknown code CollationMod"

-- | A 'Tailoring' is a collection of changes to a 'Collation'
-- that define a localization. See
-- <https://unicode.org/reports/tr35/tr35-collation.html>.
newtype Tailoring = Tailoring { unTailoring :: [CollationMod] }
  deriving (Show, Eq, Ord, Lift, Semigroup, Monoid)

instance Binary Tailoring where
   put = put . unTailoring
   get = Tailoring <$> get
