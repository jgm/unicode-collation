{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.Collate.Lang
  ( Lang(..)
  , parseLang
  , renderLang
  , lookupLang
  )
where
import Data.Maybe (listToMaybe)
import Control.Monad (mzero)
import Data.Ord (Down(..))
import Data.List (sortOn)
import Data.Char (isAlphaNum, isAscii, isDigit, isSpace, isAlpha)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as P
import Data.Binary (Binary(..))
import Data.String
import Language.Haskell.TH.Syntax (Lift(..))
import Instances.TH.Lift ()
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup (Semigroup(..))
#endif

-- | Represents a BCP 47 language tag (<https://tools.ietf.org/html/bcp47>).
data Lang = Lang{ langLanguage   :: Text
                , langScript     :: Maybe Text
                , langRegion     :: Maybe Text
                , langVariants   :: [Text]
                , langExtensions :: [(Text, [(Text , Text)])]
                , langPrivateUse :: [Text]
                } deriving (Eq, Ord, Show, Lift)

instance IsString Lang where
 fromString =
   fromRight (Lang "und" Nothing Nothing [] [] []) . parseLang . T.pack

instance Binary Lang where
 put (Lang a b c d e f) = put (a,b,c,d,e,f)
 get = do
     (a,b,c,d,e,f) <- get
     return $ Lang a b c d e f

-- | Find best match for a 'Lang' in an association list.
-- We require a match in 'langLanguage'; after that,
-- we look for matches in the following priority order:
-- 'langScript', 'langRegion', 'langVariants',
-- 'langExtensions' (under @"u"@),
-- collation ('langExtensions' under @"u-co"@).
lookupLang :: Lang -> [(Lang, a)] -> Maybe (Lang, a)
lookupLang lang =
    listToMaybe
  . sortOn (Down . scoreMatch)
  . filter (\(l,_) -> langLanguage l == langLanguage lang)
 where
  scoreMatch :: (Lang, a) -> Int
  scoreMatch (l,_) =
    (if langScript l == langScript lang then 20 else 0) +
    (if langRegion l == langRegion lang then 10 else 0) +
    (if langVariants l == langVariants lang then 5 else 0) +
    (if lookup "u" (langExtensions l) ==
        lookup "u" (langExtensions lang) then 2 else 0) +
    (if (lookup "u" (langExtensions l) >>= lookup "co") ==
        (lookup "u" (langExtensions lang) >>= lookup "co") then 1 else 0)

-- | Render a 'Lang' in BCP 47 form.
renderLang :: Lang -> Text
renderLang lang =
    langLanguage lang
     <> maybe "" (T.cons '-') (langScript lang)
     <> maybe "" (T.cons '-') (langRegion lang)
     <> mconcat (map (T.cons '-') (langVariants lang))
     <> mconcat (map renderExtension (langExtensions lang))
     <> renderPrivateUse (langPrivateUse lang)
 where
  renderExtension (c, ks) = "-" <> c <> mconcat (map renderKeyword ks)
  renderKeyword (k, v) = "-" <> k <> if T.null v
                                        then ""
                                        else "-" <> v
  renderPrivateUse [] = ""
  renderPrivateUse ts = "-x" <> mconcat (map (T.cons '-') ts)

-- | Parse a BCP 47 language tag as a 'Lang'.
parseLang :: Text -> Either String Lang
parseLang lang =
  case P.parse pLangTag "lang" (T.split (\c -> c == '-' || c == '_')
                                 $ T.takeWhile (not . isSpace) lang) of
       Right r -> Right r
       Left e  -> Left $ show e
  where
    -- langtag       = language
    --                ["-" script]
    --                ["-" region]
    --                 *("-" variant)
    --                 *("-" extension)
    pLangTag = do
      language <- pLanguage P.<?> "language"
      script <- P.option Nothing (Just <$> pScript P.<?> "script")
      region <- P.option Nothing (Just <$> pRegion P.<?> "region")
      variants <- P.many pVariant P.<?> "variant"
      extensions <- P.many pExtension P.<?> "extension"
      privateUse <- P.option [] (pPrivateUse P.<?> "private use")
      -- P.eof  -- like CSL, we allow garbage afterwards
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
    pLanguage = (do
      baselang <- T.toLower <$> alphasBetween 2 3
      extlang <- P.option Nothing $ Just <$> pExtlang
      case extlang of
        Nothing  -> pure baselang
        Just ext -> pure $ baselang <> "-" <> ext)
      P.<|> T.toLower <$> alphasBetween 4 8

    -- extlang       = 3ALPHA              ; selected ISO 639 codes
    --                 *2("-" 3ALPHA)      ; permanently reserved
    pExtlang = T.intercalate "-" <$> countBetween 1 3
                 (T.toLower <$> alphas 3)

    -- script        = 4ALPHA              ; ISO 15924 code
    pScript = T.toTitle <$> alphas 4

    -- region        = 2ALPHA              ; ISO 3166-1 code
    --               / 3DIGIT              ; UN M.49 code
    pRegion = T.toUpper <$> alphas 2 P.<|> digits 3

    -- variant       = 5*8alphanum         ; registered variants
    --              / (DIGIT 3alphanum)
    pVariant = T.toLower <$>
      (alphanumsBetween 5 8
       P.<|> tok (\t -> T.all isAsciiAlphaNum t && T.length t == 4 &&
                                    isDigit (T.head t)))

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
    pExtension = do
      c <- T.toLower <$> tok (\t -> T.length t == 1 && T.all isAsciiAlphaNum t)
      attrs <- P.many
             (T.toLower <$>
               tok (\t -> T.all isAsciiAlphaNum t && lengthBetween 3 8 t))
      keywords <- P.many pKeyword
      return (c, map (, "") attrs ++ keywords)

    pKeyword = do
      key <- alphas 2
      types <- P.many (alphanumsBetween 3 8)
      return (key, T.intercalate "-" types)

    -- privateuse    = "x" 1*("-" (1*8alphanum))
    pPrivateUse = do
      _ <- tok (\t -> T.toLower t == "x")
      P.many1 (alphanumsBetween 1 8)

    tok :: (Text -> Bool) -> P.Parsec [Text] () Text
    tok f = P.tokenPrim T.unpack (\pos t _ ->
                                   P.incSourceColumn pos (T.length t))
                      (\t -> if f t then Just t else Nothing)

    countBetween (low :: Int) (hi :: Int) p = P.try $ countBetween' low hi p 1
    countBetween' low hi p (n :: Int) = (do
     res <- p
     if n >= hi
        then return [res]
        else (res:) <$> countBetween' low hi p (n + 1))
      P.<|> (if n > low then return [] else mzero)

    isAsciiAlpha c = isAscii c && isAlpha c
    alphas len = tok (\t -> T.all isAsciiAlpha t && T.length t == len)
    digits len = tok (\t -> T.all isDigit t && T.length t == len)
    alphasBetween minLen maxLen =
      tok (\t -> T.all isAsciiAlpha t && lengthBetween minLen maxLen t)
    alphanumsBetween minLen maxLen =
      tok (\t -> T.all isAsciiAlphaNum t && lengthBetween minLen maxLen t)
    lengthBetween lo hi t = let len = T.length t in len >= lo && len <= hi
    isAsciiAlphaNum c = isAscii c && isAlphaNum c


-- We define fromRight here instead of importing it,
-- because it doesn't exist in some base versions we support.
fromRight :: b -> Either a b -> b
fromRight fallback (Left _)  = fallback
fromRight _        (Right x) = x
