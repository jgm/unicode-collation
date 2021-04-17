{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Main (main) where

import Test.Tasty.Bench
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text.ICU as ICU
import Text.Collate
import Test.QuickCheck.Instances.Text ()
import Data.List (sortBy)
-- import Debug.Trace

main :: IO ()
main = do
  let icuCollator = ICU.collator ICU.Root
  let icuCollate = ICU.collate icuCollator
  let ourCollate = collate (collatorFor "en")
  (randomTexts :: [Text]) <- generate (infiniteListOf arbitrary)
  let tenThousand = take 10000 randomTexts
  defaultMain
    [ bench "sort a list of 10000 random Texts"
        (whnf (sortBy ourCollate) tenThousand)
    , bench "sort a list of 10000 random Texts (OverloadedStrings)"
        (whnf (sortBy (collate "en")) tenThousand)
    , bench "sort same list with text-icu"
        (whnf (sortBy icuCollate) tenThousand)
    ]

