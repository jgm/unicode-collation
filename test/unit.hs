{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where
import UnicodeCollation
import UnicodeCollation.Types
import UnicodeCollation.Tailorings
import Text.Printf
import Test.Tasty
import Test.Tasty.HUnit
import Data.List (sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Data.Char
import Data.Maybe
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = do
  conformanceTree <- conformanceTests
  defaultMain (tests conformanceTree)

tests :: TestTree -> TestTree
tests conformanceTree = testGroup "Tests"
  [ conformanceTree
  , testCase "Sorting test 1" $
    sortBy ourCollate ["hi", "hit", "hít", "hat", "hot",
                       "naïve", "nag", "name"] @?=
           ["hat","hi","hit","h\237t","hot","nag","naïve","name"]
  , testCase "Sorting test 2" $
    sortBy ourCollate ["ｶ", "ヵ", "abc", "abç", "ab\xFFFE\&c", "ab©",
                       "𝒶bc", "abC", "𝕒bc", "File-3", "ガ", "が", "äbc", "カ",
                       "か", "Abc", "file-12", "filé-110"]
                      @?=
                       ["ab\xFFFE\&c", "ab©", "abc", "abC", "𝒶bc", "𝕒bc", "Abc",
                       "abç", "äbc", "filé-110", "file-12", "File-3", "か",
                       "ヵ", "カ", "ｶ", "が", "ガ"]

  , testGroup "Variable ordering test"
     $ map variableOrderingCase
      [ (NonIgnorable,
        ["de luge" ,"de Luge" ,"de-luge" ,"de-Luge" ,"de\x2010luge"
        ,"de\x2010Luge" ,"death" ,"deluge" ,"deLuge" ,"demark"])
      , (Blanked,
        ["death" ,"de luge" ,"de-luge" ,"de\x2010luge" ,"deluge"
        ,"de Luge" ,"de-Luge","de\x2010Luge", "deLuge", "demark"])
      , (Shifted,
        ["death" ,"de luge" ,"de-luge" ,"de\x2010luge" ,"deluge"
        ,"de Luge" ,"de-Luge" ,"de\x2010Luge" ,"deLuge" ,"demark"])
      , (ShiftTrimmed,
        ["death" ,"deluge" ,"de luge" ,"de-luge" ,"de\x2010luge" ,"deLuge"
        ,"de Luge" ,"de-Luge" ,"de\x2010Luge" ,"demark"])
      ]
  , testGroup "Tailoring"
    [ testCase "Inline tailoring quasiquoter 1" $
        collateWithTailoring [qTailoring|&N<ñ<<<Ñ|] "ñ" "N" @?= GT
    , testCase "Inline tailoring quasiquoter 2" $
        collateWithTailoring [qTailoring|&m<n<k|] "cake" "cane" @?= GT
    , testCase "Inline tailoring quasiquoter 3" $
        collateWithTailoring [qTailoring|&m<k<n|] "cake" "cane" @?= LT
    ]
  , testGroup "Localized collations"
    [ testCase "root cha cza" $
        collateWith "" "cha" "cza" @?= LT
    , testCase "es_traditional cha cza" $
        collateWith "es/traditional" "cha" "cza" @?= GT
    , testCase "se ö z" $
        collateWith "se" "ö" "z" @?= GT
    , testCase "tr ö z" $
        collateWith "tr" "ö" "z" @?= LT
    ]
  ]

conformanceTests :: IO TestTree
conformanceTests = do
  putStrLn "Loading conformance test data..."
  shifted <- conformanceTestsFor Shifted
              "test/uca-collation-test/CollationTest_SHIFTED_SHORT.txt"
  nonIgnorable <- conformanceTestsFor NonIgnorable
              "test/uca-collation-test/CollationTest_NON_IGNORABLE_SHORT.txt"
  return $ testGroup "Conformance tests" [nonIgnorable, shifted]


conformanceTestsFor :: VariableWeighting -> FilePath -> IO TestTree
conformanceTestsFor weighting fp = do
  xs <- parseConformanceTest fp
  let collator = mkCollator collationOptions{
                         optVariableWeighting = weighting,
                         optCollation = ducetCollation }
  return $ testGroup ("Conformance tests " ++ show weighting ++ " " ++ fp)
         $ zipWith3 (conformanceTestWith collator) (map fst xs)
                     (map snd xs) (tail (map snd xs))

conformanceTestWith :: Collator -> Int -> Text -> Text -> TestTree
conformanceTestWith collator lineNo !txt1 !txt2 =
  let showHexes = unwords . map (\c -> if c > 0xFFFF
                                          then printf "%05X" c
                                          else printf "%04X" c) .
                              map ord . T.unpack
   in testCase ("[line " ++ show lineNo ++ "] " ++
                showHexes txt1 ++ " <= " ++ showHexes txt2) $
        assertBool ("Calculated sort keys:\n" ++
                    showHexes txt1 ++ " " ++
                    (prettySortKey $ sortKey collator txt1) ++ "\n" ++
                    showHexes txt2 ++ " " ++
                    (prettySortKey $ sortKey collator txt2))
                   (collate collator txt1 txt2 /= GT)

collateWithTailoring :: Tailoring -> Text -> Text -> Ordering
collateWithTailoring tlrng =
  collate (mkCollator collationOptions{ optCollation =
                                         rootCollation `withTailoring` tlrng })

collateWith :: Text -> Text -> Text -> Ordering
collateWith spec =
  collate (mkCollator collationOptions{ optCollation =
                            localizedCollation spec })

variableOrderingCase :: (VariableWeighting , [Text]) -> TestTree
variableOrderingCase (w , expected) =
  testCase (show w) $
     sortBy (collate (mkCollator collationOptions{
                        optVariableWeighting = w }))
           -- from Table 12
           [ "de luge"
           , "de Luge"
           , "de-luge"
           , "de-Luge"
           , "de\x2010luge"
           , "de\x2010Luge"
           , "death"
           , "deluge"
           , "deLuge"
           , "demark" ]
           @?= expected

ourCollate :: Text -> Text -> Ordering
ourCollate =
  collate ourCollator

ourCollator :: Collator
ourCollator = mkCollator collationOptions{ optVariableWeighting = Shifted }

parseConformanceTest :: FilePath -> IO [(Int, Text)]
parseConformanceTest fp = do
  bs <- B8.readFile fp
  let beginsWithHexDigit = maybe False (isHexDigit . fst) . B8.uncons
  let allLines = B8.lines bs
  let prologue = takeWhile (not . beginsWithHexDigit) allLines
  let lns = drop (length prologue) allLines
  let firstLine = 1 + length prologue
  return $ catMaybes $ zipWith parseConformanceTestLine [firstLine..] lns

parseConformanceTestLine :: Int -> B8.ByteString -> Maybe (Int, Text)
parseConformanceTestLine lineno bs =
  let readhex = either error fst . TR.hexadecimal
      codepoints = map (readhex . TE.decodeLatin1) $ B8.words bs
   in if B8.take 1 bs == "#"
         then Nothing
         else Just (lineno, T.pack $ map chr codepoints)

prettySortKey :: SortKey -> String
prettySortKey sk = T.unpack $
  "[" <>
   T.strip
    (T.unwords
      [tohexes (sortL1 sk), "|",
       tohexes (sortL2 sk), "|",
       tohexes (sortL3 sk), "|",
       tohexes (sortL4 sk), "|"])
  <> "]"
 where
  tohexes = T.unwords . map tohex
  tohex = T.pack . printf "%04X"

{-
icuCollate :: Text -> Text -> Ordering
icuCollate = ICU.collate icuCollator

icuSortKey :: Text -> String
icuSortKey = concatMap (printf "%02X ") . B.unpack . ICU.sortKey icuCollator

icuCollator :: ICU.Collator
icuCollator = ICU.collatorWith ICU.Root
                 [ ICU.Collate.AlternateHandling ICU.Collate.Shifted
                 , ICU.Collate.NormalizationMode True
                 , ICU.Collate.Strength ICU.Collate.Quaternary]

agreesWithICU :: TextPairInRange -> Bool
agreesWithICU (TextPairInRange a b) = ourCollate a b == icuCollate a b

ducetCollator :: Collator
ducetCollator = mkCollator collationOptions{ optCollation = ducetCollation }

toHex :: Text -> [String]
toHex = map (printf "%04X") . T.unpack
-}
