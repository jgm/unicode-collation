{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import UnicodeCollation
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.List (sortBy)
import System.Environment (getArgs)
import Data.Maybe
import Control.Monad
import System.Exit

main :: IO ()
main = do
  args <- getArgs

  let isHelp "-h"     = True
      isHelp "--help" = True
      isHelp _        = False

  when (any isHelp args) $ do
    putStrLn "Usage:    unicode-collate [COLLATION]"
    putStrLn "Options:"
    putStrLn "          --help  Print usage information"
    putStrLn "          --list  List supported collations"
    putStrLn ""
    putStrLn "Sorts lines from stdin using the specified collation."
    putStrLn "COLLATION is a BCP47 language code. Examples:"
    putStrLn "unicode-collate               # Sort lines from stdin using root collation"
    putStrLn "unicode-collate es            # Use standard Spanish collation"
    putStrLn "unicode-collate es-u-co-trad  # Use traditional Spanish"
    putStrLn "unicode-collate fr-CA         # Use Canadian French collation"
    exitSuccess

  when ("--list" `elem` args) $ do
    mapM_ (T.putStrLn . renderLang . fst) tailorings
    exitSuccess

  spec <- maybe "root" T.pack . listToMaybe <$> getArgs
  lang <- either error return $ parseLang spec
  let myCollator = collatorFor lang
  T.getContents >>= mapM_ T.putStrLn . sortBy (collate myCollator) . T.lines

