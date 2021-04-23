{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Text.Collate.UnicodeData
  ( parseUnicodeData
  , toCanonicalCombiningClassMap
  , toCanonicalDecompositionMap
  , genCanonicalCombiningClassMap
  , genCanonicalDecompositionMap
  , readCodePoints
  )
where
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import qualified Data.IntMap as M
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qAddDependentFile)

unicodeDataPath :: FilePath
unicodeDataPath = "data/UnicodeData.txt"

readUtf8Text :: FilePath -> IO Text
readUtf8Text fp = TE.decodeUtf8 <$> B.readFile fp

-- | Generate map of code points to canonical combining class,
-- from UnicodeData.txt.
genCanonicalCombiningClassMap :: Q Exp
genCanonicalCombiningClassMap = do
  qAddDependentFile unicodeDataPath
  cccmap <- toCanonicalCombiningClassMap . parseUnicodeData
               <$> runIO (readUtf8Text unicodeDataPath)
  [| cccmap |]

-- | Generate map of code points to canonical decompositions,
-- from UnicodeData.txt.
genCanonicalDecompositionMap :: Q Exp
genCanonicalDecompositionMap = do
  qAddDependentFile unicodeDataPath
  dmap <- toCanonicalDecompositionMap . parseUnicodeData
              <$> runIO (readUtf8Text unicodeDataPath)
  [| dmap |]

parseUnicodeData :: Text -> M.IntMap UChar
parseUnicodeData = foldr parseLine mempty . T.lines

toCanonicalCombiningClassMap :: M.IntMap UChar -> M.IntMap Int
toCanonicalCombiningClassMap =
  fmap uCanonicalCombiningClass .  M.filter ((> 0) . uCanonicalCombiningClass)

toCanonicalDecompositionMap :: M.IntMap UChar -> M.IntMap [Int]
toCanonicalDecompositionMap =
  fmap uDecompositionMapping
   . M.filter (\x -> uDecompositionType x == Canonical &&
                     not (null (uDecompositionMapping x)))

data GeneralCategory =
  Lu | Ll | Lt | Lm | Lo |
  Mn | Mc | Me |
  Nd | Nl | No |
  Pc | Pd | Ps | Pe | Pi | Pf | Po |
  Sm | Sc | Sk | So |
  Zs | Zl | Zp |
  Cc | Cf | Cs | Co | Cn
  deriving (Show, Read, Eq, Ord, Enum, Generic)

data BidiClass =
  L | LRE | LRO | R | AL | RLE | RLO | PDF | EN | ES |
  ET | AN | CS | NSM | BN | B | S | WS | ON |
  LRI | RLI | FSI | PDI
  deriving (Show, Read, Eq, Ord, Enum, Generic)

data DecompositionType =
  Font | NoBreak | Initial | Medial | Final | Isolated |
  Circle | Super | Sub | Vertical | Wide | Narrow | Small |
  Square | Fraction | Compat | Canonical
  deriving (Show, Read, Eq, Ord, Enum, Generic)

data UChar =
  UChar
  { uCodePoint :: Int
  , uName :: Text
  , uGeneralCategory :: GeneralCategory
  , uCanonicalCombiningClass :: Int
  , uBidiClass :: BidiClass
  , uDecompositionType :: DecompositionType
  , uDecompositionMapping :: [Int]
  , uNumericTypeAndValue :: (Maybe Int, Maybe Int, Maybe Int)
  , uBidiMirrored :: Bool
  , uUnicode1Name :: Text
  , uISOComment :: Text
  , uSimpleUppercaseMapping :: Int
  , uSimpleLowercaseMapping :: Int
  , uSimpleTitlecaseMappping :: Int
  } deriving (Show, Eq, Ord, Generic)

readCodePoint :: Text -> Int
readCodePoint t =
  case TR.hexadecimal t of
    Left e               -> error e -- ok to error at compile-time
    Right (codepoint, _) -> codepoint

readCodePoints :: Text -> ([Int], Text)
readCodePoints t =
  case TR.hexadecimal t of
    Left _                  -> ([], t)
    Right (codepoint, rest) ->
      let (cps, t') = readCodePoints (T.dropWhile (==' ') rest)
        in (codepoint:cps, t')

parseDecomp :: Text -> (DecompositionType, [Int])
parseDecomp bs =
  case T.uncons bs of
    Just ('<',rest) -> (ty,xs)
      where
       xs = fst $ readCodePoints cps
       (x,y) = T.break (=='>') rest
       cps = T.dropWhile (\c -> c == '>' || c == ' ') y
       ty = case x of
              "font" -> Font
              "noBreak" -> NoBreak
              "initial" -> Initial
              "medial" -> Medial
              "final" -> Final
              "isolate" -> Isolated
              "circle" -> Circle
              "super" -> Super
              "sub" -> Sub
              "vertical" -> Vertical
              "wide" -> Wide
              "narrow" -> Narrow
              "small" -> Small
              "square" -> Square
              "fraction" -> Fraction
              "compat" -> Compat
              _ -> Compat
    _ -> (Canonical,) . fst $ readCodePoints bs

parseLine :: Text -> M.IntMap UChar -> M.IntMap UChar
parseLine t =
  case T.splitOn ";" t of
    [f0,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14] ->
      M.insert codepoint uchar
     where
      codepoint = readCodePoint f0
      (decompType, decompMapping) = parseDecomp f5
      readNumericValue x =
        case TR.decimal x of
          Left _      -> Nothing
          Right (v,_) -> Just v
      uchar = UChar
        { uCodePoint = codepoint
        , uName = f1
        , uGeneralCategory = read (T.unpack f2)
        , uCanonicalCombiningClass = either (const 0) fst (TR.decimal f3)
        , uBidiClass = read (T.unpack f4)
        , uDecompositionType = decompType
        , uDecompositionMapping = decompMapping
        , uNumericTypeAndValue =
             (readNumericValue f6,
              readNumericValue f7,
              readNumericValue f8)
        , uBidiMirrored = f9 == "Y"
        , uUnicode1Name = f10
        , uISOComment = f11
        , uSimpleUppercaseMapping = readCodePoint f12
        , uSimpleLowercaseMapping = readCodePoint f13
        , uSimpleTitlecaseMappping = readCodePoint f14
        }
    _ -> error $ "Wrong number of fields in record:\n" ++ show t


