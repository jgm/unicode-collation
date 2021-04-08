module UnicodeCollation.Lang
  ( Lang(..)
  , parseLang
  , renderLang
  )
where
import Control.Monad (guard)
import Data.Char (isAlphaNum, isAscii, isLetter, isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as P


-- | Represents BCP 47 language code.
data Lang = Lang{ langLanguage   :: Text
                , langScript     :: Maybe Text
                , langRegion     :: Maybe Text
                , langVariants   :: [Text]
                , langExtensions :: [(Char, [Text])]
                } deriving (Eq, Ord, Show)

-- | Render a Lang in BCP 47.
renderLang :: Lang -> Text
renderLang lang =
    langLanguage lang
     <> maybe "" (T.cons '-') (langScript lang)
     <> maybe "" (T.cons '-') (langRegion lang)
     <> mconcat (map (T.cons '-') (langVariants lang))
     <> mconcat (map renderExtension (langExtensions lang))
 where
  renderExtension (c, ts) =
    "-" <> T.singleton c <> mconcat (map (T.cons '-') ts)

-- | Parse a BCP 47 string as a Lang.  Currently we parse
-- extensions and private-use fields as "variants," even
-- though officially they aren't.
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
      P.eof
      return Lang{   langLanguage = language
                   , langScript = script
                   , langRegion = region
                   , langVariants = variants
                   , langExtensions = extensions }
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

    asciiLetter      = P.satisfy (\c -> isAscii c && isLetter c)
    asciiLetterLower = P.satisfy (\c -> isAscii c && isLetter c && isLower c)
    asciiLetterUpper = P.satisfy (\c -> isAscii c && isLetter c && isUpper c)
    separ = () <$ (P.char '-' P.<|> P.char '_')

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
      ds <- ((++) <$> (P.count 1 P.digit) <*> (P.count 3 P.alphaNum))
         P.<|> ((++) <$> (P.count 5 P.alphaNum) <*> (P.many P.alphaNum))
      return $ T.pack ds

    -- extension     = singleton 1*("-" (2*8alphanum))
    pExtension = P.try $ do
      separ
      c <- P.alphaNum
      attrs <- P.many1 (separ *> P.many1 P.alphaNum)
      pure (c, map T.pack attrs)

