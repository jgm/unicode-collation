{-# LANGUAGE ScopedTypeVariables #-}
module UnicodeCollation.Lang
  ( Lang(..)
  , parseLang
  , renderLang
  )
where
import Control.Monad (void, mzero)
import Data.Char (isAlphaNum, isAscii, isLetter, isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as P

-- | Represents BCP 47 language code.
data Lang = Lang{ langLanguage   :: Text
                , langScript     :: Maybe Text
                , langRegion     :: Maybe Text
                , langVariants   :: [Text]
                , langExtensions :: [(Char, [(Text , Maybe Text)])]
                , langPrivateUse :: [Text]
                } deriving (Eq, Ord, Show)

-- | Render a Lang in BCP 47.
renderLang :: Lang -> Text
renderLang lang =
    langLanguage lang
     <> maybe "" (T.cons '-') (langScript lang)
     <> maybe "" (T.cons '-') (langRegion lang)
     <> mconcat (map (T.cons '-') (langVariants lang))
     <> mconcat (map renderExtension (langExtensions lang))
     <> renderPrivateUse (langPrivateUse lang)
 where
  renderExtension (c, ks) =
    "-" <> T.singleton c <> mconcat (map renderKeyword ks)
  renderKeyword (k, Nothing) = "-" <> k
  renderKeyword (k, Just v) = "-" <> k <> "-" <> v
  renderPrivateUse ts =
    "-x" <> mconcat (map (T.cons '-') ts)

-- | Parse a BCP 47 string as a Lang.
parseLang :: Text -> Either Text Lang
parseLang lang =
  case P.parse pLangTag "lang" lang of
       Right r -> Right r
       Left e  -> Left $ T.pack $ show e
  where
    -- langtag       = language
    --                ["-" script]
    --                ["-" region]
    --                 *("-" variant)
    --                 *("-" extension)
    pLangTag = do
      language <- pLanguage
      script <- P.option Nothing $ Just <$> pScript
      region <- P.option Nothing $ Just <$> pRegion
      variants <- P.many pVariant
      extensions <- P.many pExtension
      privateUse <- pPrivateUse
      P.eof
      return Lang{   langLanguage = language
                   , langScript = script
                   , langRegion = region
                   , langVariants = variants
                   , langExtensions = extensions
                   , langPrivateUse = privateUse }
    -- language      = 2*3ALPHA            ; shortest ISO 639 code
    --                 ["-" extlang]       ; sometimes followed by
    --                                     ; extended language subtags
    --               / 4ALPHA              ; or reserved for future use
    --               / 5*8ALPHA            ; or registered language subtag
    pLanguage = do
      cs <- P.many1 asciiLetter
      let lcs = length cs
      let baselang = T.toLower $ T.pack cs
      case lcs of
        n | n < 2     -> fail "language too short"
          | n < 4     -> ((baselang <>) <$> pExtlang) P.<|> pure baselang
          | n < 9     -> pure baselang
          | otherwise -> fail "language too long"

    -- extlang       = 3ALPHA              ; selected ISO 639 codes
    --                 *2("-" 3ALPHA)      ; permanently reserved
    pExtlang = P.try $ do
      _ <- separ
      cs <- T.pack <$> P.count 3 asciiLetterLower
      others <- P.many (P.try (separ *>
                                (T.pack <$> P.count 3 asciiLetterLower)))
      pure $ "-" <> T.intercalate "-" (cs:others)

    -- script        = 4ALPHA              ; ISO 15924 code
    pScript = P.try $ do
      separ
      x <- asciiLetterUpper
      xs <- P.count 3 asciiLetterLower
      return $ T.pack (x:xs)

    -- region        = 2ALPHA              ; ISO 3166-1 code
    --               / 3DIGIT              ; UN M.49 code
    pRegion = P.try $ do
      separ
      cs <- P.count 2 asciiLetterUpper P.<|> P.count 3 P.digit
      return $ T.pack cs

    -- variant       = 5*8alphanum         ; registered variants
    --              / (DIGIT 3alphanum)
    pVariant = P.try $ do
      separ
      ds <- countRange 5 8 asciiAlphaNum P.<|>
             ((++) <$> (P.count 1 P.digit) <*> (P.count 3 asciiAlphaNum))
      return $ T.pack ds

    -- extension     = singleton 1*("-" (2*8alphanum))
    -- RFC6087:
    -- An 'attribute' is a subtag with a length of three to eight
    -- characters following the singleton and preceding any 'keyword'
    -- sequences.  No attributes were defined at the time of this
    -- document's publication.

    -- A 'keyword' is a sequence of subtags consisting of a 'key' subtag,
    -- followed by zero or more 'type' subtags (so a 'key' might appear
    -- alone and not be accompanied by a 'type' subtag).  A 'key' MUST
    -- NOT appear more than once in a language tag's extension string.
    -- The order of the 'type' subtags within a 'keyword' is sometimes
    -- significant to their interpretation.

    -- A.  A 'key' is a subtag with a length of exactly two characters.
    --     Each 'key' is followed by zero or more 'type' subtags.

    -- B.  A 'type' is a subtag with a length of three to eight
    --     characters following a 'key'.  'Type' subtags are specific to
    --     a particular 'key' and the order of the 'type' subtags MAY be
    --     significant to the interpretation of the 'keyword'.
    pExtension = P.try $ do
      separ
      c <- P.alphaNum
      attrs <- P.many (separ *> countRange 3 8 asciiAlphaNum)
      keywords <- P.many (separ *> pKeyword)
      return (c, map (\attr -> (T.pack attr, Nothing)) attrs ++ keywords)

    pKeyword = P.try $ do
      key <- P.count 2 asciiLetterLower
      types <- P.many (P.try (separ *> countRange 3 8 asciiAlphaNum))
      return (T.pack key,
              case types of
                     [] -> Nothing
                     _  -> Just (T.intercalate "-" $ map T.pack types))

    -- privateuse    = "x" 1*("-" (1*8alphanum))
    pPrivateUse = P.try $ do
      separ
      void $ P.char 'x'
      P.many1 (separ *> (T.pack <$> countRange 1 8 asciiAlphaNum))

    asciiLetter      = P.satisfy (\c -> isAscii c && isLetter c)
    asciiLetterLower = P.satisfy (\c -> isAscii c && isLetter c && isLower c)
    asciiLetterUpper = P.satisfy (\c -> isAscii c && isLetter c && isUpper c)
    asciiAlphaNum    = P.satisfy (\c -> isAscii c && isAlphaNum c)
    separ = () <$ (P.char '-' P.<|> P.char '_')

    countRange (low :: Int) (hi :: Int) p = P.try $ countRange' low hi p 1
    countRange' low hi p (n :: Int) = (do
      res <- p
      if n >= hi
         then return [res]
         else (res:) <$> countRange' low hi p (n + 1))
      P.<|> (if n > low then return [] else mzero)


