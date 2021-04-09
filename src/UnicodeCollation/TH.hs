{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module UnicodeCollation.TH
  ( genCollation
  , genTailorings
  , genTailoring
  )
where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qAddDependentFile)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Binary as Binary ( encode )
import UnicodeCollation.Collation (parseCollation)
import UnicodeCollation.Mods (parseCollationXMLs, parseTailoring)
import UnicodeCollation.Types
import UnicodeCollation.Lang
import Data.Text (Text)
import qualified Data.Text as T
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup (Semigroup(..))
#endif

-- import Debug.Trace

-- NOTE: The reason for the indirection through binary
-- is that including a string literal in the sources instead
-- of a large structured object (e.g. a Map) dramatically
-- reduces compile times.  This seems a flaw in GHC and when
-- it is addressed, we could switch to a more straightforward
-- method.

genCollation :: FilePath -> Q Exp
genCollation fp = do
  qAddDependentFile fp
  binaryRep <- Binary.encode . parseCollation <$> runIO (B.readFile fp)
  return $ LitE $ StringL $ BL.unpack binaryRep

genTailorings :: FilePath -> Q [Dec]
genTailorings fp = do
  ts <- runIO (parseCollationXMLs fp)
  tailoringDecs <- mconcat <$> mapM mkTailoringDec ts
  lookupTable <- mkLookupTable $ map fst ts
  return $ lookupTable ++ tailoringDecs

mkLookupTable :: [Lang] -> Q [Dec]
mkLookupTable colnames = do
  let toColPair lang = do
        cn <- [| lang |]
        return $
#if MIN_VERSION_template_haskell(2,16,0)
          TupE [ Just cn
               , Just (VarE (mkCollationName lang))
               ]
#else
          TupE [ cn
               , VarE (mkCollationName lang)
               ]
#endif
  colpairs <- mapM toColPair colnames
  ttype <- [t| [(Lang, Tailoring)] |]
  return [ SigD (mkName "tailorings") ttype
         , FunD (mkName "tailorings")
             [Clause [] (NormalB
                          (ListE colpairs)) []]
         ]

mkTailoringDec :: (Lang, Tailoring) -> Q [Dec]
mkTailoringDec (lang, mods) = do
  let name = mkCollationName lang
  let binaryRep = Binary.encode mods
  return [ SigD name (ConT (mkName "Tailoring"))
         , FunD name [Clause [] (NormalB
                                  (AppE
                                    (VarE (mkName "decode"))
                                    (LitE (StringL (BL.unpack binaryRep))))) []]
         ]

mkCollationName :: Lang -> Name
mkCollationName lang =
  mkName . T.unpack $ "tailoring_" <> cleanName (renderLang lang)
 where
  cleanName = T.map (\c -> if c == '-' then '_' else c)

genTailoring :: Text -> Q Exp
genTailoring t =
  case parseTailoring "inline" t of
    Left e     -> fail $ "Could not parse inline tailoring:\n" <> show e
    Right mods -> [| mods |]

