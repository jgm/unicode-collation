{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module UnicodeCollation.Tailorings
where

import UnicodeCollation.Types
import UnicodeCollation.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import UnicodeCollation.Mods (applyCollationMod)
import Data.Text (Text)
import Data.Map (Map, fromList)
import qualified Data.Map as M
import Data.List (foldl')
import Data.Binary (decode)
import qualified Data.Text as T

-- This generates function declarations for all of our collations.
$(genTailorings "data/collation")

-- | Enable the @QuasiQuotes@ language extension and then
-- create a tailoring at compile time: e.g., @[qTailoring|&b < a < d]@.
-- For the synatx, see <https://unicode.org/reports/tr35/tr35-collation.html>.
qTailoring :: QuasiQuoter
qTailoring = QuasiQuoter
  { quoteExp = genTailoring . T.pack
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

-- | Apply a 'Tailoring' to a 'Collation'.
withTailoring :: Collation -> Tailoring -> Collation
withTailoring collation (Tailoring mods) =
  foldl' applyCollationMod collation mods

-- | Look up a 'Tailoring'.  The first element of the tuple
-- is the language (BCP 47), the second is the collation name
-- (or @Nothing@ for the language's default collation).
lookupTailoring :: (Text, Maybe Text) -> Maybe Tailoring
lookupTailoring (lang, mbcol) = M.lookup (lang, mbcol) tailorings
