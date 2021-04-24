{-# LANGUAGE TemplateHaskell #-}
module Text.Collate.CanonicalCombiningClass
  ( canonicalCombiningClass )
where
import Text.Collate.UnicodeData (genCanonicalCombiningClassMap)
import qualified Data.IntMap as M
import Data.Maybe (fromMaybe)

-- | Map from code points to canonical combining classes.
-- Only nonzero elements are included.
combiningClassMap :: M.IntMap Int
combiningClassMap = $(genCanonicalCombiningClassMap)

-- | Determine the canonical combining class for a code point.
canonicalCombiningClass :: Int -> Int
canonicalCombiningClass cp
  | cp < 0x300 = 0  -- optimization
  | otherwise  = fromMaybe 0 . M.lookup cp $! combiningClassMap


