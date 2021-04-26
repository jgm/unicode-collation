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
import Text.Collate.Normalize (toNFD)
import Test.QuickCheck.Instances.Text ()
import Data.List (sortBy)
import Data.Char (isAscii, ord, chr)

main :: IO ()
main = do
  (randomTexts :: [Text]) <- generate (infiniteListOf arbitrary)
  (randomLatinStrings :: [String]) <-
      generate (infiniteListOf (listOf (elements latinChars)))
  (randomAsciiTexts :: [Text]) <-
    generate (infiniteListOf (arbitrary `suchThat` T.all isAscii))
  let tenThousand = take 10000 randomTexts
  let tenThousandLatin = map T.pack $ take 10000 randomLatinStrings
  let tenThousandLatinNFD = map (T.pack . map chr . toNFD . map ord . T.unpack)
                              tenThousandLatin
  let tenThousandString = map T.unpack tenThousand
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
    , bench "sort a list of 10000 Texts (composed latin) (en)"
        (whnf (sortBy (collate (collatorFor "en"))) tenThousandLatin)
    , bench "sort same list with text-icu (en)"
        (whnf (sortBy (ICU.collate (icuCollator "en"))) tenThousandLatin)
    , bench "sort same list but pre-normalized (en-u-kk-false)"
        (whnf (sortBy (collate (collatorFor "en-u-kk-false"))) tenThousandLatinNFD)
    , bench "sort a list of 10000 ASCII Texts (en)"
        (whnf (sortBy (collate (collatorFor "en"))) tenThousandAscii)
    , bench "sort same list with text-icu (en)"
        (whnf (sortBy (ICU.collate (icuCollator "en"))) tenThousandAscii)
    , bench "sort a list of 10000 random Texts that agree in first 32 chars"
        (whnf (sortBy (collate (collatorFor "en"))) tenThousandLong)
    , bench "sort same list with text-icu (en)"
        (whnf (sortBy (ICU.collate (icuCollator "en"))) tenThousandLong)
    , bench "sort a list of 10000 identical Texts (en)"
        (whnf (sortBy collateString) (replicate 10000 "ḀḁḂḃḄḅḆḇḈḉḊḋḌḍḎḏḐḑḒḓḔ"))
    , bench "sort a list of 10000 random Strings (en)"
        (whnf (sortBy collateString) tenThousandString)
    ]

latinChars :: [Char]
latinChars = "ḀḁḂḃḄḅḆḇḈḉḊḋḌḍḎḏḐḑḒḓḔḕḖḗḘḙḚḛḜḝḞḟḠḡḢḣḤḥḦḧḨḩḪḫḬḭḮḯḰḱḲḳḴḵḶḷḸḹḺḻḼḽḾḿṀṁṂṃṄṅṆṇṈṉṊṋṌṍṎṏṐṑṒṓṔṕṖṗṘṙṚṛṜṝṞṟṠṡṢṣṤṥṦṧṨṩṪṫṬṭṮṯṰṱṲṳṴṵṶṷṸṹṺṻṼṽṾṿ‐‑‒–—―‖‗‘’‚‛“”„‟†‡•‣․‥…"
