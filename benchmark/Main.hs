{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Main (main) where

import Test.Tasty.Bench
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.ICU as ICU
import Data.Text.ICU.Collate (Attribute(..), Strength(..))
import Text.Collate
import Test.QuickCheck.Instances.Text ()
import Data.List (sortBy)
import Data.Char (isAscii)

main :: IO ()
main = do
  (randomTexts :: [Text]) <- generate (infiniteListOf arbitrary)
  (randomSingletonTexts :: [Text]) <-
    generate (infiniteListOf (arbitrary `suchThat` (\t -> T.length t == 1)))
  (randomAsciiTexts :: [Text]) <-
    generate (infiniteListOf (arbitrary `suchThat` T.all isAscii))
  let tenThousand = take 10000 randomTexts
  let tenThousandString = map T.unpack tenThousand
  let tenThousandSingletons = take 10000 randomSingletonTexts
  let tenThousandAscii = take 10000 randomAsciiTexts
  let tenThousandLong = map ("A bcd efgh ijklmnop qrs tuv WxyZ" <>) tenThousand
  let icuCollator lang = ICU.collatorWith (ICU.Locale lang)
                          [NormalizationMode True, Strength Quaternary]
  let collateString = collateWithUnpacker (collatorFor "en") id
  defaultMain
    [ bench "sort a list of 10000 random Texts (en)"
        (whnf (sortBy (collate (collatorFor "en"))) tenThousand)
    , bench "sort same list with text-icu (en)"
        (whnf (sortBy (ICU.collate (icuCollator "en"))) tenThousand)
    , bench "sort a list of 10000 random Texts (zh)"
        (whnf (sortBy (collate (collatorFor "zh"))) tenThousand)
    , bench "sort same list with text-icu (zh)"
        (whnf (sortBy (ICU.collate (icuCollator "zh"))) tenThousand)
    , bench "sort a list of 10000 random Strings (en)"
        (whnf (sortBy collateString) tenThousandString)
    , bench "sort a list of 10000 ASCII Texts (en)"
        (whnf (sortBy (collate (collatorFor "en"))) tenThousandAscii)
    , bench "sort same list with text-icu (en)"
        (whnf (sortBy (ICU.collate (icuCollator "en"))) tenThousandAscii)
    , bench "sort a list of 10000 random Texts (en-u-kk-false = no normalize)"
        (whnf (sortBy (collate (collatorFor "en-u-kk-false"))) tenThousand)
    , bench "sort a list of 10000 random Texts of length 1 (en)"
        (whnf (sortBy (collate (collatorFor "en"))) tenThousandSingletons)
    , bench "sort same list with text-icu (en)"
        (whnf (sortBy (ICU.collate (icuCollator "en"))) tenThousandSingletons)
    , bench "sort a list of 10000 random Texts that agree in first 32 chars (en)"
        (whnf (sortBy (collate (collatorFor "en"))) tenThousandLong)
    , bench "sort same list with text-icu (en)"
        (whnf (sortBy (ICU.collate (icuCollator "en"))) tenThousandLong)
    ]
