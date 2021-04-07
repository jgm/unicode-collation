{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import UnicodeCollation
import UnicodeCollation.Tailorings (tailorings)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List (sortBy)
import System.Environment (getArgs)
import Data.Maybe
import Control.Monad
import System.Exit
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup (Semigroup(..))
#endif

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
    putStrLn "COLLATION is a BCP47 language code, followed optionally"
    putStrLn "by / and a collation name.  Examples:"
    putStrLn "unicode-collate # Sort lines from stdin using root collation"
    putStrLn "unicode-collate es # Use standard Spanish collation"
    putStrLn "unicode-collate es/traditional # Use traditional Spanish"
    putStrLn "unicode-collate fr-CA # Use Canadian French collation"
    exitSuccess

  let printCollation (lang, mbcoll) =
        T.putStrLn (maybe lang (\coll -> lang <> "/" <> coll) mbcoll)

  when ("--list" `elem` args) $ do
    mapM_ printCollation $ M.keys tailorings
    exitSuccess

  spec <- maybe mempty T.pack . listToMaybe <$> getArgs
  let myCollator = collate $
                     mkCollator collationOptions{
                        optCollation = localizedCollation spec }
  T.getContents >>= mapM_ T.putStrLn . sortBy myCollator . T.lines

