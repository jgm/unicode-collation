{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Text.Collate
import Data.Char (chr, ord)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.List (sortBy, find)
import System.Environment (getArgs)
import Control.Monad
import System.Exit
import Text.Printf
import System.IO
import Data.Text.Normalize (normalize, NormalizationMode(NFD))

main :: IO ()
main = do
  args <- getArgs

  let isHelp "-h"     = True
      isHelp "--help" = True
      isHelp _        = False

  when (any isHelp args) $ do
    putStrLn "Usage:    unicode-collate [COLLATION]"
    putStrLn "Options:"
    putStrLn "          --hex     Parse input as hex code points"
    putStrLn "          --help    Print usage information"
    putStrLn "          --list    List supported collations"
    putStrLn "          --verbose Include diagnostic information"
    putStrLn ""
    putStrLn "Sorts lines from stdin using the specified collation."
    putStrLn "COLLATION is a BCP47 language code. Examples:"
    putStrLn "unicode-collate                 # Use root collation"
    putStrLn "unicode-collate es              # Use standard Spanish collation"
    putStrLn "unicode-collate es-u-co-trad    # Use traditional Spanish"
    putStrLn "unicode-collate fr-CA           # Use Canadian French collation"
    putStrLn "unicode-collate fr-u-kb         # Use reverse accent order"
    putStrLn "unicode-collate fr-u-ka-shifted # Use Shifted variable weighting"
    exitSuccess

  when ("--list" `elem` args) $ do
    mapM_ (T.putStrLn . renderLang . fst) tailorings
    exitSuccess

  let isOpt ('-':_) = True
      isOpt _       = False
  spec <- maybe "root" T.pack . find (not . isOpt) <$> getArgs
  lang <- either handleError return $ parseLang spec
  let myCollator = collatorFor lang
  let verbose = "--verbose" `elem` args
  let codepoints = "--hex" `elem` args
  let opts = collatorOptions myCollator
  when verbose $ do
    hPutStrLn stderr "Options:"
    hPutStrLn stderr $ "  Tailoring:          " <>
                    maybe "ROOT" (T.unpack . renderLang) (optLang opts)
    hPutStrLn stderr $ "  Variable weighting: " <>
                      show (optVariableWeighting opts)
    hPutStrLn stderr $ "  French accents:     " <>
                      show (optFrenchAccents opts)
    hPutStrLn stderr $ "  Upper before lower: " <>
                      show (optUpperBeforeLower opts)
    hPutStrLn stderr $ "  Normalize:          " <>
                    show (optNormalize opts)
  let renderLine t = do
        t' <- if codepoints
                 then parseAsCodePoints t
                 else return t
        when verbose $
          hPutStrLn stderr $ renderCodePoints (normalize NFD t') ++ "; # ("
            ++ T.unpack t' ++ ") " ++ renderSortKey (sortKey myCollator t')
        T.putStrLn t'
  T.getContents >>= mapM_ renderLine . sortBy (collate myCollator) . T.lines

renderCodePoints :: Text -> String
renderCodePoints t =
  unwords $ map (printf "%04X" . ord) (T.unpack t)

parseAsCodePoints :: Text -> IO Text
parseAsCodePoints t = do
  let ws = T.words $ T.takeWhile (/=';') t -- everything after ; is ignored
  cs <- mapM parseCodePoint ws
  return $ T.pack $ map chr cs

parseCodePoint :: Text -> IO Int
parseCodePoint t =
  case TR.hexadecimal t of
    Right (x,t') | T.null t' -> return x
    _ -> handleError $ "Could not parse " <> show t <> " as hex code point."

handleError :: String -> IO a
handleError msg = do
  hPutStrLn stderr msg
  exitWith $ ExitFailure 1
