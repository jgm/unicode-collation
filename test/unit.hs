{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where
import Text.Collate
import Text.Collate.Normalize (toNFD)
import Text.Printf
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Data.Either (lefts)
import Data.List (sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.Text.Normalize as N
import Data.Char
import Data.Maybe
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = do
  conformanceTree <- conformanceTests
  coverageTree <- implicitWeightCoverageTests
  defaultMain (tests conformanceTree coverageTree)

tests :: TestTree -> TestTree -> TestTree
tests conformanceTree coverageTree = testGroup "Tests"
  [ conformanceTree
  , coverageTree
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
    , testCase "zh-u-co-pinyin" $
        collate "zh-u-co-pinyin" "\x963F" "\x5475"  @?= LT
    , testCase "zh-u-co-stroke" $
        collate "zh-u-co-stroke" "\x963F" "\x5475"  @?= GT
    ]
  , testCase "QuasiQuotes" $
       collate [collator|zh-u-co-pinyin|] "\x963F" "\x5475" @?= LT
  , testGroup "BCP 47 Lang parsing"
       (map langParseTest langPairs)
  , testGroup "BCP 47 Lang round-trip"
       (map langRoundTripTest langPairs)
  , testGroup "Lang fallback behavior"
    [ testCase "de => ROOT" $
      (optLang . collatorOptions) "de" @?= Nothing
    , testCase "de-AT => ROOT" $
        (optLang . collatorOptions) "dt-AT" @?= Nothing
    , testCase "es-u-co-trad" $
        (optLang . collatorOptions) "es-ES" @?=
          Just (Lang "es" Nothing Nothing [] [] [])
    , testCase "de-DE-u-co-phonebk" $
        (optLang . collatorOptions) "de-DE-u-co-phonebk" @?=
          Just (Lang "de" Nothing Nothing [] [("u",[("co","phonebk")])] [])
    , testCase "es-u-co-nonexist-kb" $
        (optLang . collatorOptions) "es-u-co-nonexist-kb" @?=
          Just (Lang "es" Nothing Nothing [] [] [])
    ]
  , testGroup "Normalization"
    [ testProperty "toNFD agrees with unicode-transforms"
         (\cs -> toNFD (map ord cs) ==
           (map ord . T.unpack . N.normalize N.NFD . T.pack) cs)
    ]
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
  return $ testCase ("Conformance tests " ++ show weighting ++ " " ++ fp)
         $ (\zs -> case lefts zs of
                     [] -> return ()
                     es -> assertFailure (unlines es))
         $ map (conformanceTestWith coll)
              (zip3 (map fst xs) (map snd xs) (drop 1 (map snd xs)))

-- | Guard against the hand-coded ideograph ranges in
-- 'Text.Collate.Collation' (used by 'calculateImplicitWeight') drifting
-- out of sync with the shipped Unicode data.  Every code point that the
-- data marks as an ideograph must receive an /ideographic/ implicit
-- primary weight; anything not covered by the hand-coded ranges falls to
-- the generic unassigned bucket at @0xFBC0@ and beyond.  We enumerate the
-- full set (not just range endpoints) so that adding a new ideograph block
-- to the data without updating the ranges is caught.
--
-- Sources of truth, both in the shipped data:
--
--   * @\@implicitweights@ directives in @data\/allkeys.txt@
--     (Tangut, Nushu, Khitan -> 0xFB00..0xFB02)
--   * @\<... Ideograph ..., First\/Last\>@ markers in
--     @data\/UnicodeData.txt@ (CJK Unified + extensions -> 0xFB40\/0xFB80)
implicitWeightCoverageTests :: IO TestTree
implicitWeightCoverageTests = do
  putStrLn "Loading implicit-weight coverage data..."
  allkeys <- B8.readFile "data/allkeys.txt"
  ucd     <- B8.readFile "data/UnicodeData.txt"
  let ranges = implicitWeightDirectives allkeys ++ ideographRanges ucd
      cps    = concatMap (\(lo, hi) -> [lo .. hi]) ranges
  return $ testCase "Implicit weights cover all ideographs in shipped data" $
    case [ (cp, w) | cp <- cps, let w = implicitPrimary cp, w >= 0xFBC0 ] of
      []  -> return ()
      bad -> assertFailure $
               "Code points marked as ideographs in the data receive the "
            ++ "generic\n(>= 0xFBC0) implicit weight; the hand-coded ranges "
            ++ "in\nText.Collate.Collation are stale (" ++ show (length bad)
            ++ " affected):\n"
            ++ unlines [ printf "  U+%05X -> primary %04X" cp w
                       | (cp, w) <- take 40 bad ]

-- | Primary weight assigned to a single code point (head of its sort key).
implicitPrimary :: Int -> Int
implicitPrimary cp =
  case sortKey rootCollator (T.singleton (chr cp)) of
    SortKey (w : _) -> fromIntegral w
    SortKey []      -> 0

-- | Parse the @\@implicitweights LO..HI; WEIGHT@ directives from
-- @allkeys.txt@ into inclusive code-point ranges.
implicitWeightDirectives :: B8.ByteString -> [(Int, Int)]
implicitWeightDirectives = mapMaybe parseLine . B8.lines
 where
  parseLine ln =
    let t = TE.decodeLatin1 ln
     in if "@implicitweights" `T.isPrefixOf` t
           then dotRange (T.takeWhile (/= ';') (T.drop 16 t))
           else Nothing
  dotRange t =
    case T.splitOn ".." (T.strip t) of
      [a, b] -> (,) <$> hexInt a <*> hexInt b
      _      -> Nothing

-- | Parse the @\<... Ideograph ..., First\/Last\>@ range markers from
-- @UnicodeData.txt@ into inclusive code-point ranges.
ideographRanges :: B8.ByteString -> [(Int, Int)]
ideographRanges = pairUp . mapMaybe marker . B8.lines
 where
  marker ln =
    case T.splitOn ";" (TE.decodeLatin1 ln) of
      (cpF : nameF : _)
        | "Ideograph" `T.isInfixOf` nameF
        , Just cp <- hexInt cpF
        -> if      ", First>" `T.isSuffixOf` nameF then Just (cp, False)
           else if ", Last>"  `T.isSuffixOf` nameF then Just (cp, True)
           else Nothing
      _ -> Nothing
  pairUp ((lo, False) : (hi, True) : rest) = (lo, hi) : pairUp rest
  pairUp (_ : rest)                        = pairUp rest
  pairUp []                                = []

-- | Parse a (possibly space-padded) hexadecimal integer, requiring that it
-- consume the whole input.
hexInt :: Text -> Maybe Int
hexInt t =
  case TR.hexadecimal (T.strip t) of
    Right (n, rest) | T.null (T.strip rest) -> Just n
    _                                       -> Nothing

conformanceTestWith :: Collator -> (Int, Text, Text) -> Either String ()
conformanceTestWith coll (lineNo, txt1, txt2) =
  let showHexes = unwords . map ((\c -> if c > 0xFFFF
                                           then printf "%05X" c
                                           else printf "%04X" c) . ord)
                          . T.unpack
   in if collate coll txt1 txt2 /= GT
         then Right ()
         else Left $ "[line " ++ show lineNo ++ "] " ++
                      showHexes txt1 ++ " <= " ++ showHexes txt2 ++ "\n" ++
                      "  Calculated sort keys:\n  [" ++
                        showHexes txt1 ++ "] " ++
                        renderSortKey (sortKey coll txt1) ++ "\n  [" ++
                        showHexes txt2 ++ "] " ++
                        renderSortKey (sortKey coll txt2)

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
ourCollator = setVariableWeighting Shifted rootCollator

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
      t = T.pack (map chr codepoints)
   in if T.any (== '\xFFFD') t || T.null t
         then Nothing
         else Just (lineno, t)
