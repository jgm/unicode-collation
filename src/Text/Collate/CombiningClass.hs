{-# LANGUAGE TemplateHaskell #-}
module Text.Collate.CombiningClass
  ( canonicalCombiningClass )
where
import Text.Collate.UnicodeData (genCanonicalCombiningClassMap)
import qualified Data.IntMap as M
import Data.Maybe (fromMaybe)

combiningClassMap :: M.IntMap Int
combiningClassMap = $(genCanonicalCombiningClassMap)

-- | Determine the canonical combining class for a code point.
canonicalCombiningClass :: Int -> Int
canonicalCombiningClass cp
  | cp < 0x300 = 0
  | otherwise  = fromMaybe 0 . M.lookup cp $! combiningClassMap


