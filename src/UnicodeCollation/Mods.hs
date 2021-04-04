{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module UnicodeCollation.Mods
  ( parseTailoring
  , applyCollationMod
  , parseCollationXML
  , parseCollationXMLs
  )
where
import Data.Maybe
import Control.Monad (mplus)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.ByteString as B
import UnicodeCollation.Types
import UnicodeCollation.Elements (getCollationElements)
import Text.Parsec
import Text.Parsec.Text
import Data.Char (chr, isSpace, ord, isLetter)
import Control.Monad (void)
import Text.HTML.TagSoup (Tag(..))
import Text.HTML.TagSoup.Tree (parseTree, universeTree, TagTree(..))
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, (</>))
import Language.Haskell.TH.Syntax (Quasi(..))
import qualified Data.Text.Normalize as N
-- import Debug.Trace

parseTailoring :: String -> Text -> Either ParseError Tailoring
parseTailoring fp =
  parse (Tailoring . concat <$> (pSkippable *> many pCollationMods <* eof)) fp

lexeme :: Parser a -> Parser a
lexeme p =  p <* pSkippable

pTarget :: Parser Target
pTarget = try
  (do void $ lexeme $ char '['
      constructor <- (TargetFirst <$ string "first")
                 <|> (TargetLast <$ string "last")
      category' <- T.strip . T.pack <$> many1 (satisfy (/= ']')) <* char ']'
      category <- case category' of
                        "tertiary ignorable" -> pure TertiaryIgnorable
                        "secondary ignorable" -> pure SecondaryIgnorable
                        "primary ignorable" -> pure PrimaryIgnorable
                        "variable" -> pure Variable
                        "regular" -> pure Regular
                        "implicit" -> pure Implicit
                        "trailing" -> pure Trailing
                        _ -> fail $ "Unknown category " ++ T.unpack category'
      return $ constructor category)
  <|> (TargetText <$> pText)


pCollationMods :: Parser [CollationMod]
pCollationMods = do
  void pSkippable
  void $ char '&'
  spaces
  beforeLevel <- option Nothing $ try $ do
                    void $ lexeme $ char '['
                    void $ lexeme $ string "before"
                    d <- lexeme $ digit
                    void $ lexeme $ char ']'
                    return $
                      case d of
                        '1' -> Just L1
                        '2' -> Just L2
                        '3' -> Just L3
                        '4' -> Just L4
                        _   -> Nothing

  firstText <- lexeme pTarget
  (rels, texts) <- unzip . concat <$>
                    many1 (do (rel, starred) <- lexeme pRel
                              t <- lexeme pText
                              if starred
                                 then return $ map (rel,) (splitText t)
                                 else return [(rel, t)])
  let rels' = case (beforeLevel, rels) of
                (Just lvl, _:rest) -> Before lvl : rest
                _                  -> rels
  return $ zipWith3 (\f x y -> f x y)
                    rels' (firstText : map TargetText texts)
                    (map TargetText texts)

pRel :: Parser (Target -> Target -> CollationMod, Bool)
pRel = do
  raw <- many1 (char '<') <|> string "="
  star <- option False $ True <$ char '*'
  case raw of
    "="    -> return (Equal, star)
    "<"    -> return (After L1, star)
    "<<"   -> return (After L2, star)
    "<<<"  -> return (After L3, star)
    "<<<<" -> return (After L4, star)
    _      -> fail  $ "Unknown relation " ++ raw

splitText :: Text -> [Text]
splitText =
  map T.singleton . fillGaps . T.unpack
 where
  fillGaps [] = []
  fillGaps (c:'-':d:xs) | d > c = enumFromTo c d ++ fillGaps xs
  fillGaps (c:cs) = c : fillGaps cs


pSkippable :: Parser ()
pSkippable = skipMany $
  void space <|> pComment <|> pBracketed <|>
  void (char '\x200E') -- left-to-right mark

pBracketed :: Parser ()
pBracketed = do
  void $ lexeme $ char '['
  void $ many $ satisfy (\c -> c /= '[' && c /= ']')
  optional $ lexeme $ pBracketed
  void $ lexeme $ char ']'

pComment :: Parser ()
pComment = char '#' *> skipMany (satisfy (/= '\n'))

pText :: Parser Text
pText = mconcat <$>
  many (pQuoted <|>
        T.pack <$> many1 (pEscaped <|> pEscQuote <|> satisfy (not . isSpecial)))

pEscaped :: Parser Char
pEscaped = do
  void $ char '\\'
  c <- oneOf "uUtnr"
  let toHexChar = either (const '\xFFFD') (chr . fst) .
                   TR.hexadecimal . T.pack
  case c of
    'u' -> toHexChar <$> count 4 hexDigit
    'U' -> toHexChar <$> count 8 hexDigit
    't' -> return '\t'
    'n' -> return '\n'
    'r' -> return '\r'
    'f' -> return '\f'
    '"' -> return '"'
    _   -> fail $ "Unknown escape \\" ++ [c]


pQuoted :: Parser Text
pQuoted = do
  void $ char '\''
  T.pack <$> manyTill (pEscQuote <|> anyChar) (char '\'')

pEscQuote :: Parser Char
pEscQuote = try $ char '\'' >> char '\''


isSpecial :: Char -> Bool
isSpecial '<' = True
isSpecial '=' = True
isSpecial '*' = True
isSpecial '[' = True
isSpecial ']' = True
isSpecial '&' = True
isSpecial '#' = True
isSpecial '\'' = True
isSpecial '\\' = True
isSpecial '"' = True
isSpecial c | isSpace c = True
isSpecial _ = False

{-
   From the spec:

  - Find the last collation element whose strength is at least as great as the
    strength of the operator. For example, for << find the last primary or
    secondary CE. This CE will be modified; all following CEs should be removed.
    If there is no such CE, then reset the collation elements to a single
    completely-ignorable CE.

  - Increment the collation element weight corresponding to the strength of the
    operator. For example, for << increment the secondary weight.  The new
    weight must be less than the next weight for the same combination of
    higher-level weights of any collation element according to the current
    state.

  - Weights must be allocated in accordance with the UCA well-formedness
    conditions.

  - When incrementing any weight, lower-level weights should be reset to the
    “common” values, to help with sort key compression.

-}


applyCollationMod :: Collation -> CollationMod -> Collation
applyCollationMod collation cmod =
  case cmod of
    After lvl a b  ->
      case (getTarget a, getTarget b) of
        (Just a', Just b') ->
          alterElements (reorder 1 lvl (getCollationElements collation a'))
                         b'
                         collation
        _ -> collation
    Before lvl a b  ->
      case (getTarget a, getTarget b) of
        (Just a', Just b') ->
          alterElements (reorder (-1) lvl (getCollationElements collation a'))
                         b'
                         collation
        _ -> collation
    Equal a b      ->
      case (getTarget a, getTarget b) of
        (Just a', Just b') ->
          insertElements b' (getCollationElements collation a') collation
        _ -> collation

 where

  getTarget (TargetText t) = Just $ textToCodePoints t
  getTarget (TargetFirst Implicit) =
                                fst <$> findFirst (>= firstImplicit) collation
  getTarget (TargetFirst cat) = fst <$> findFirst (`hasCategory` cat) collation
  getTarget (TargetLast Regular) = fst <$> findFirst (>= firstHani) collation
  getTarget (TargetLast Implicit) = fst <$> findLast (<= lastImplicit) collation
  getTarget (TargetLast cat) = fst <$> findLast (`hasCategory` cat) collation

  firstHani = [CollationElement False 0xFB40 0 0 0]

  lastImplicit = [CollationElement False 0xFBE1 0xFFFF 0xFFFF 0xFFFF]

  firstImplicit = [CollationElement False 0xFB00 0 0 0]

  textToCodePoints = map ord . T.unpack . N.normalize N.NFD

  reorder :: Int    -- increment/decrement
          -> Level
          -> [CollationElement]
          -> Maybe [CollationElement]
          -> Maybe [CollationElement]
  reorder _ _   []   _ = Just [completelyIgnorable]
  reorder n lvl (a:as) (Just [])
    | levelOf a <= lvl = Just [incrementLevel n lvl a a]
    | otherwise        = reorder n lvl as (Just [])
  reorder n lvl (a:as) (Just (b:bs))
    | levelOf a <= lvl = Just [incrementLevel n lvl a b]
    | otherwise        = (b :) <$> reorder n lvl as (Just bs)
  reorder n lvl (a:_) Nothing = Just [incrementLevel n lvl a a]

  incrementLevel n L1 eltA eltB =
    eltB{ collationL1 = fromIntegral $ fromIntegral (collationL1 eltA) + n }
  incrementLevel n L2 eltA eltB =
    eltB{ collationL1 = collationL1 eltA
        , collationL2 = fromIntegral $ fromIntegral (collationL2 eltA) + n }
  incrementLevel n L3 eltA eltB =
    eltB{ collationL1 = collationL1 eltA
        , collationL2 = collationL2 eltA
        , collationL3 = fromIntegral $ fromIntegral (collationL3 eltA) + n }
  incrementLevel n L4 eltA _eltB =
    eltA{ collationL4 = fromIntegral $ fromIntegral (collationL4 eltA) + n }

  levelOf (CollationElement _ l1 l2 l3 _) =
    if l1 > 0
       then L1
       else if l2 > 0
               then L2
               else if l3 > 0
                       then L3
                       else L4

  completelyIgnorable = CollationElement False 0 0 0 0xFFFF -- is this right ?

parseCollationXML :: Text -> [((Text, Maybe Text), Text)]
parseCollationXML t =
  ((lang, Nothing), defaultCollation) :
  [((lang, Just collname), txt)
    | (collname, txt) <- collations ]
  where
   lang = language <> maybe "" ("-" <>) territory <>
                      maybe "" ("-" <>) script <>
                      maybe "" ("-" <>) variant
   univ = universeTree $ parseTree t
   language = fromMaybe "UNKNOWN" $
               listToMaybe [l | TagBranch "language" [("type", l)] _ <- univ]
   territory = listToMaybe [x | TagBranch "territory" [("type", x)] _ <- univ]
   script = listToMaybe [x | TagBranch "script" [("type", x)] _ <- univ]
   variant = listToMaybe [x | TagBranch "variant" [("type", x)] _ <- univ]
   defaultCollationName = fromMaybe "standard" $
       listToMaybe [extractText xs | TagBranch "defaultCollation" _ xs <- univ]
   defaultCollation = fromMaybe "" $ listToMaybe
       [extractText xs | TagBranch "collation" [("type", ty)] xs <- univ
                       , ty == defaultCollationName]
   collations = [(ty, extractText xs)
                  | TagBranch "collation" [("type", ty)] xs <- univ]
   extractText xs = mconcat [txt | TagLeaf (TagText txt) <- universeTree xs]


parseCollationXMLs :: FilePath -> IO [((Text, Maybe Text), Tailoring)]
parseCollationXMLs dir = do
  fs <- map (dir </>) . filter ((== ".xml") . takeExtension)
         <$> getDirectoryContents dir
  rawmap <- concat <$>
            mapM (fmap (parseCollationXML . TE.decodeUtf8) . B.readFile) fs
  let handleImports txt = do
        let (x,y) =  T.breakOn "[import " txt
        if T.null y
           then return txt
           else do
             let (target, rest) = T.break (==']') (T.drop 8 y)
             let (lang, rest') = T.break (=='-') $
                                  case T.stripPrefix "und" target of
                                    Nothing -> target
                                    Just tt -> "root" <> tt
             let baselang = T.takeWhile isLetter lang
             let coll =
                   case T.stripPrefix "-u-co-" rest' of
                     Nothing -> Nothing
                     Just "trad" -> Just "traditional"
                     Just "phonebk" -> Just "phonebook"
                     Just z -> Just z
             interp <-
               case lookup (lang, coll) rawmap `mplus`
                    lookup (baselang, coll) rawmap of
                  Nothing -> do
                      qReport False $ "Could not import " <> show target
                      return ""
                  Just t -> return t
             ((x <> interp) <>) <$> handleImports (T.drop 1 rest)

  let toCollationMods ((lang,mbcol), txt) =
        do txt' <- handleImports txt
           let colname = maybe lang ((lang <> "/") <>) mbcol
           case parseTailoring (T.unpack colname) txt' of
              Left e    -> do
                qReport False $ "Unable to parse " <> T.unpack colname <> ":"
                qReport True $ show e
                return Nothing
              Right mds -> return $ Just ((lang,mbcol), mds)
  catMaybes <$> mapM toCollationMods rawmap
