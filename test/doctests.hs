import Test.DocTest
import System.Environment (setEnv)

main :: IO ()
main = do
  setEnv "LC_ALL" "C.UTF-8"
  doctest ["-isrc", "src/UnicodeCollation.hs"]
