{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Main (main) where

import Test.Tasty.Bench
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text.ICU as ICU
import Data.Text.ICU.Collate (Attribute(..), Strength(..))
import Text.Collate
import Test.QuickCheck.Instances.Text ()
import Data.List (sortBy)
-- import Debug.Trace

main :: IO ()
main = do
  (randomTexts :: [Text]) <- generate (infiniteListOf arbitrary)
  let tenThousand = take 10000 randomTexts
  let icuCollator lang = ICU.collatorWith (ICU.Locale lang)
                          [NormalizationMode True, Strength Quaternary]
  defaultMain
    [ bench "sort a list of 10000 random Texts (en)"
        (whnf (sortBy (collate (collatorFor "en"))) tenThousand)
    , bench "sort same list with text-icu (en)"
        (whnf (sortBy (ICU.collate (icuCollator "en"))) tenThousand)
    , bench "sort a list of 10000 random Texts (zh)"
        (whnf (sortBy (collate (collatorFor "zh"))) tenThousand)
    , bench "sort same list with text-icu (zh)"
        (whnf (sortBy (ICU.collate (icuCollator "zh"))) tenThousand)
    , bench "sort a list of 10000 random Texts (en-u-kk-false = no normalize)"
        (whnf (sortBy (collate (collatorFor "en-u-kk-false"))) tenThousand)
    ]

