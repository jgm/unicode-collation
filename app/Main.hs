{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Text.Collate
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.List (sortBy)
import System.Environment (getArgs)
import Data.Maybe
import Control.Monad
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs

  let isHelp "-h"     = True
      isHelp "--help" = True
      isHelp _        = False

  when (any isHelp args) $ do
    putStrLn "Usage:    unicode-collate [COLLATION]"
    putStrLn "Options:"
    putStrLn "          --help     Print usage information"
    putStrLn "          --list     List supported collations"
    putStrLn "          --verbose  Include diagnostic information"
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
  spec <- maybe "root" T.pack . listToMaybe . filter (not . isOpt) <$> getArgs
  lang <- either handleError return $ parseLang spec
  let myCollator = collatorFor lang
  let verbose = "--verbose" `elem` args
  when verbose $
    T.putStrLn $ "Using tailoring: " <>
      maybe "ROOT" renderLang (collatorLang myCollator)
  let renderLine t = do
        T.putStr t
        when verbose $
          T.putStr $ " " <> T.pack (renderSortKey (sortKey myCollator t))
        T.putStrLn ""
  T.getContents >>= mapM_ renderLine . sortBy (collate myCollator) . T.lines

handleError :: String -> IO a
handleError msg = do
  hPutStrLn stderr msg
  exitWith $ ExitFailure 1
