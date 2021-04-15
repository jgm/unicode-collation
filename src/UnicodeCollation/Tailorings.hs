{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module UnicodeCollation.Tailorings
 ( tailorCollation
 , ducetCollation
 , tailor
 , tailorings )
where

import UnicodeCollation.Types
import UnicodeCollation.TH
import UnicodeCollation.Mods (applyCollationMod)
import Data.List (foldl')
import Data.Binary (decode)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Data.Text as T

-- | Apply a 'Tailoring' to a 'Collation'.
tailorCollation :: Collation -> Tailoring -> Collation
tailorCollation collation (Tailoring mods) =
  foldl' applyCollationMod collation mods

-- | The DUCET collation defined in allkeys.txt.
ducetCollation :: Collation
ducetCollation = decode $(genCollation "data/allkeys.txt")

-- | Create a tailoring at compile time: e.g., @[tailor|&b < a < d]@.
-- Requires the @QuasiQuotes@ extension.
-- For the syntax, see <https://unicode.org/reports/tr35/tr35-collation.html>.
tailor :: QuasiQuoter
tailor = QuasiQuoter
  { quoteExp = genTailoring . T.pack
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

-- This generates function declarations for all of our collations.
$(genTailorings "data/collation")

