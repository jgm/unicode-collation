{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveLift #-}
module UnicodeCollation.Types
  ( CollationOptions(..)
  , VariableWeighting(..)
  , CollationElement(..)
  , Collation(..)
  , SortKey(..)
  )
where

import Data.Word (Word16)
import Data.Binary (Binary(get, put))
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
  , optUpperBeforeLower   :: Bool -- ^ Sort uppercase letters before lower
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

instance Binary Collation where
   put (Collation m) = put m
   get = Collation <$> get

showWordList :: [Word16] -> String
showWordList ws =
    "[" ++ intercalate ","
            (map (printf "0x%04X" . (fromIntegral :: Word16 -> Int)) ws) ++ "]"

newtype SortKey = SortKey [Word16]
  deriving (Eq, Ord)

instance Show SortKey where
 show (SortKey ws) = "SortKey " ++ showWordList ws

-- Note that & b < q <<< Q is the same as & b < q, & q <<< Q
-- Another syntactic shortcut is:
-- & a <* bcd-gp-s => & a < b < c < d < e < f < g < p < q < r < s
-- & a =* bB => & a = b = B (without that, we have a contraction)
-- &[before 2] a << b => sorts sorts b before a

