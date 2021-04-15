{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where
import UnicodeCollation
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
    sortBy (collate ourCollator) ["hi", "hit", "hít", "hat", "hot",
                       "naïve", "nag", "name"] @?=
           ["hat","hi","hit","h\237t","hot","nag","naïve","name"]
  , testCase "Sorting test 2" $
    sortBy (collate ourCollator)
                      ["ｶ", "ヵ", "abc", "abç", "ab\xFFFE\&c", "ab©",
                       "𝒶bc", "abC", "𝕒bc", "File-3", "ガ", "が", "äbc", "カ",
                       "か", "Abc", "file-12", "filé-110"]
                      @?=
                       ["ab©", "abc", "abC", "𝒶bc", "𝕒bc", "Abc",
                       "abç", "äbc", "ab\xFFFE\&c",
                       "filé-110", "file-12", "File-3",
                       "か", "ヵ", "カ", "ｶ", "が", "ガ"]

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
        collateWithTailoring [tailor|&N<ñ<<<Ñ|] "ñ" "N" @?= GT
    , testCase "Inline tailoring quasiquoter 2" $
        collateWithTailoring [tailor|&m<n<k|] "cake" "cane" @?= GT
    , testCase "Inline tailoring quasiquoter 3" $
        collateWithTailoring [tailor|&m<k<n|] "cake" "cane" @?= LT
    -- , testCase "Inline tailoring quasiquoter 4" $
    --     sortBy (collateWithTailoring [tailor|&a<z/e &ae<x|])
    --       ["ae", "x", "af", "ag", "az", "z", "b"] @?=
    --       ["ae", "x", "af", "ag", "az", "z", "b"]
    ]
  , testGroup "Localized collations"
    [ testCase "root cha cza" $
        collate "und" "cha" "cza" @?= LT
    , testCase "es traditional cha cza" $
        collate "es-u-co-trad" "cha" "cza" @?= GT
    , testCase "se ö z" $
        collate "se" "ö" "z" @?= GT
    , testCase "tr ö z" $
        collate "tr" "ö" "z" @?= LT
    , testCase "fr-CA sorted list" $
        sortBy (collate (collatorFor "fr-CA-u-kb-true"))
        ["déjà","Meme", "deja", "même", "dejà", "bpef", "bœg", "Boef", "Mémé",
         "bœf", "boef", "bnef", "pêche", "pèché", "pêché", "pêche", "pêché"]
         @?=
        ["bnef", "boef", "Boef", "bœf", "bœg", "bpef", "deja", "dejà", "déjà",
         "Meme", "même", "Mémé", "pêche", "pêche", "pèché", "pêché", "pêché"]
    , testCase "fr with French accents" $
        collate "fr-u-kb-true" "coté" "côte" @?= GT
    , testCase "fr without French accents" $
        collate "fr-u-kb-false" "coté" "côte" @?= LT
    , testCase "fr kb defaults to true" $
        collate "fr-u-kb" "coté" "côte" @?= GT
    , testCase "fr without kb defaults to false" $
        collate "fr" "coté" "côte" @?= LT
    , testCase "en with shifted" $
        collate "en-u-ka-shifted" "de-luge" "de Luge" @?= LT
    , testCase "en with nonignorable" $
        collate "en-u-ka-noignore" "de-luge" "de Luge" @?= GT
    , testCase "de-u-co-phonebk" $
        sortBy (collate "de-u-co-phonebk")
        ["Übelacker", "Üxküll", "Uell", "Ülle", "Udet", "Uffenbach", "Ueve"]
        @?=
        ["Udet", "Übelacker", "Uell", "Ülle", "Ueve", "Üxküll", "Uffenbach"]
    ]
  , testGroup "BCP 47 Lang parsing"
       (map langParseTest langPairs)
  , testGroup "BCP 47 Lang round-trip"
       (map langRoundTripTest langPairs)
  ]

emptyLang :: Lang
emptyLang = Lang mempty mempty mempty mempty mempty mempty

langPairs :: [(Text, Lang)]
langPairs = [ ("en", emptyLang{langLanguage = "en"})
            , ("en with garbage following",
                     emptyLang{langLanguage = "en"})
            , ("en--with garbage following", -- CSL allows this
                     emptyLang{langLanguage = "en"})
            , ("en-US", emptyLang{langLanguage = "en", langRegion = Just "US"})
            , ("sr_Latn_RS", emptyLang{langLanguage = "sr",
                                   langScript = Just "Latn",
                                   langRegion = Just "RS"})
            , ("es-419", emptyLang{langLanguage = "es",
                                   langRegion = Just "419"})
            , ("de-CH-1996", emptyLang{langLanguage = "de",
                                       langRegion = Just "CH",
                                       langVariants = ["1996"]})
            , ("en-u-kr-latin-digit", emptyLang{langLanguage = "en",
                     langExtensions = [("u", [("kr", "latin-digit")])]})
            ]

langParseTest :: (Text, Lang) -> TestTree
langParseTest (t, l) =
  testCase (T.unpack t) $ parseLang t @?= Right l

langRoundTripTest :: (Text, Lang) -> TestTree
langRoundTripTest (_,l) =
  let l' = renderLang l
   in testCase (T.unpack l') $ renderLang <$> parseLang l' @?= Right l'

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
  let coll = setVariableWeighting weighting rootCollator
  return $ testGroup ("Conformance tests " ++ show weighting ++ " " ++ fp)
         $ zipWith3 (conformanceTestWith coll) (map fst xs)
                     (map snd xs) (tail (map snd xs))

conformanceTestWith :: Collator -> Int -> Text -> Text -> TestTree
conformanceTestWith coll lineNo !txt1 !txt2 =
  let showHexes = unwords . map ((\c -> if c > 0xFFFF
                                           then printf "%05X" c
                                           else printf "%04X" c) . ord)
                          . T.unpack
   in testCase ("[line " ++ show lineNo ++ "] " ++
                showHexes txt1 ++ " <= " ++ showHexes txt2) $
        assertBool ("Calculated sort keys:\n" ++
                    showHexes txt1 ++ " " ++
                    prettySortKey (sortKey coll txt1) ++ "\n" ++
                    showHexes txt2 ++ " " ++
                    prettySortKey (sortKey coll txt2))
                   (collate coll txt1 txt2 /= GT)

collateWithTailoring :: Tailoring -> Text -> Text -> Ordering
collateWithTailoring tlrng =
  collate (rootCollator `withTailoring` tlrng)

variableOrderingCase :: (VariableWeighting , [Text]) -> TestTree
variableOrderingCase (w , expected) =
  testCase (show w) $
     sortBy (collate (setVariableWeighting w rootCollator))
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

ourCollator :: Collator
ourCollator = setVariableWeighting Shifted $ rootCollator

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
prettySortKey (SortKey ws) = tohexes ws
 where
  tohexes = unwords . map tohex
  tohex = printf "%04X"

