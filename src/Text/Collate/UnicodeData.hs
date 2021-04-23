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
  )
where
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import qualified Data.IntMap as M
import GHC.Generics (Generic)
import Data.ByteString.Lex.Integral (readHexadecimal, readDecimal)
import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qAddDependentFile)

unicodeDataPath :: FilePath
unicodeDataPath = "data/UnicodeData.txt"

-- | Generate map of code points to canonical combining class,
-- from UnicodeData.txt.
genCanonicalCombiningClassMap :: Q Exp
genCanonicalCombiningClassMap = do
  qAddDependentFile unicodeDataPath
  cccmap <- toCanonicalCombiningClassMap . parseUnicodeData
               <$> runIO (B.readFile unicodeDataPath)
  [| cccmap |]

-- | Generate map of code points to canonical decompositions,
-- from UnicodeData.txt.
genCanonicalDecompositionMap :: Q Exp
genCanonicalDecompositionMap = do
  qAddDependentFile unicodeDataPath
  dmap <- toCanonicalDecompositionMap . parseUnicodeData
              <$> runIO (B.readFile unicodeDataPath)
  [| dmap |]

parseUnicodeData :: ByteString -> M.IntMap UChar
parseUnicodeData = foldr parseLine mempty . B.lines

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
  , uName :: ByteString
  , uGeneralCategory :: GeneralCategory
  , uCanonicalCombiningClass :: Int
  , uBidiClass :: BidiClass
  , uDecompositionType :: DecompositionType
  , uDecompositionMapping :: [Int]
  , uNumericTypeAndValue :: (Maybe Int, Maybe Int, Maybe Int)
  , uBidiMirrored :: Bool
  , uUnicode1Name :: ByteString
  , uISOComment :: ByteString
  , uSimpleUppercaseMapping :: Int
  , uSimpleLowercaseMapping :: Int
  , uSimpleTitlecaseMappping :: Int
  } deriving (Show, Eq, Ord, Generic)

readCodepoint :: ByteString -> Int
readCodepoint b =
  case readHexadecimal b of
    Nothing -> 0
    Just (codepoint, _) -> codepoint

readCodepoints :: ByteString -> ([Int], ByteString)
readCodepoints b =
  case readHexadecimal b of
    Nothing -> ([], b)
    Just (codepoint, rest) ->
      let (cps, b') = readCodepoints (B.dropWhile (==' ') rest)
        in (codepoint:cps, b')

parseDecomp :: ByteString -> (DecompositionType, [Int])
parseDecomp bs =
  case B.uncons bs of
    Just ('<',rest) -> (ty,xs)
      where
       xs = fst $ readCodepoints cps
       (x,y) = B.break (=='>') rest
       cps = B.dropWhile (\c -> c == '>' || c == ' ') y
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
    _ -> (Canonical,) . fst $ readCodepoints bs

parseLine :: ByteString -> M.IntMap UChar -> M.IntMap UChar
parseLine bs =
  case B.split ';' bs of
    [f0,f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14] ->
      M.insert codepoint uchar
     where
      codepoint = readCodepoint f0
      (decompType, decompMapping) = parseDecomp f5
      uchar = UChar
        { uCodePoint = codepoint
        , uName = f1
        , uGeneralCategory = read (B.unpack f2)
        , uCanonicalCombiningClass = maybe 0 fst (readDecimal f3)
        , uBidiClass = read (B.unpack f4)
        , uDecompositionType = decompType
        , uDecompositionMapping = decompMapping
        , uNumericTypeAndValue = (fst <$> readDecimal f6,
                                  fst <$> readDecimal f7,
                                  fst <$> readDecimal f8)
        , uBidiMirrored = f9 == "Y"
        , uUnicode1Name = f10
        , uISOComment = f11
        , uSimpleUppercaseMapping = readCodepoint f12
        , uSimpleLowercaseMapping = readCodepoint f13
        , uSimpleTitlecaseMappping = readCodepoint f14
        }
    _ -> error $ "Wrong number of fields in record:\n" ++ show bs

