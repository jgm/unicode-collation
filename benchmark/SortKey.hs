{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Benchmark comparing two ways of sorting with a 'Collator':
--
--   * naive:   @sortBy (collate coll)@         -- recomputes both sort keys
--                                                 on every comparison
--   * keyed:   @sortOn (sortKey coll)@         -- computes each sort key once
--
-- Inputs are generated with a self-contained xorshift PRNG so this target
-- depends only on the library, text, deepseq and tasty-bench (no text-icu /
-- QuickCheck).  Results are forced with 'nf' so the whole sort is measured.
module Main (main) where

import Test.Tasty.Bench
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Bits (xor, shiftL, shiftR)
import Data.Char (chr)
import Data.List (sortBy, sortOn)
import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Collate

-- ---------------------------------------------------------------------------
-- A tiny deterministic PRNG (xorshift64), so benchmarks are reproducible
-- and we avoid a QuickCheck dependency.

xorshift :: Word64 -> Word64
xorshift s0 =
  let s1 = s0 `xor` (s0 `shiftL` 13)
      s2 = s1 `xor` (s1 `shiftR` 7)
  in  s2 `xor` (s2 `shiftL` 17)

randoms :: Word64 -> [Word64]
randoms = drop 1 . iterate xorshift

-- Map a Word64 to a Char in [lo,hi] (inclusive code point range).
type CharGen = Word64 -> Char

rangeGen :: Int -> Int -> CharGen
rangeGen lo hi w = chr (lo + fromIntegral (w `mod` fromIntegral (hi - lo + 1)))

-- Build n Texts. Each text has a length drawn from [minLen,maxLen] and
-- characters produced by the given CharGen.
buildTexts :: Int -> (Int, Int) -> CharGen -> [Word64] -> [Text]
buildTexts 0 _ _ _ = []
buildTexts n (lo, hi) g ws0 =
  case ws0 of
    [] -> []
    (lenW : ws) ->
      let len          = lo + fromIntegral (lenW `mod` fromIntegral (hi - lo + 1))
          (chunk, ws') = splitAt len ws
          txt          = T.pack (map g chunk)
      in txt : buildTexts (n - 1) (lo, hi) g ws'

-- A text whose first 25 chars are a shared prefix (worst case for the naive
-- sort: every comparison must scan past the identical prefix before it can
-- decide, forcing nearly the whole sort key each time).
withSharedPrefix :: Text -> Text
withSharedPrefix = ("The quick brown fox jumps" <>)

-- ---------------------------------------------------------------------------
-- The two sorting strategies.

naiveSort :: Collator -> [Text] -> [Text]
naiveSort coll = sortBy (collate coll)

keyedSort :: Collator -> [Text] -> [Text]
keyedSort coll = sortOn (sortKey coll)

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  let coll = collatorFor "en"
      n    = 10000

  -- Four data shapes, each a forced list of 10000 Texts.
  ascii <- evaluate $ force $
             buildTexts n (1, 20) (rangeGen 0x61 0x7A) (randoms 0x1234)
  latin <- evaluate $ force $   -- Latin Extended Additional: need NFD
             buildTexts n (1, 20) (rangeGen 0x1E00 0x1EFF) (randoms 0x5678)
  cjk   <- evaluate $ force $   -- implicit weights path
             buildTexts n (1, 20) (rangeGen 0x4E00 0x9FFF) (randoms 0x9ABC)
  long  <- evaluate $ force $ map withSharedPrefix $
             buildTexts n (1, 20) (rangeGen 0x61 0x7A) (randoms 0xDEF0)

  defaultMain
    [ bgroup "ascii"
        [ bench "naive  (collate)" $ nf (naiveSort coll) ascii
        , bench "keyed  (sortKey)" $ nf (keyedSort coll) ascii
        ]
    , bgroup "latin (needs NFD)"
        [ bench "naive  (collate)" $ nf (naiveSort coll) latin
        , bench "keyed  (sortKey)" $ nf (keyedSort coll) latin
        ]
    , bgroup "cjk (implicit weights)"
        [ bench "naive  (collate)" $ nf (naiveSort coll) cjk
        , bench "keyed  (sortKey)" $ nf (keyedSort coll) cjk
        ]
    , bgroup "shared-prefix (worst case)"
        [ bench "naive  (collate)" $ nf (naiveSort coll) long
        , bench "keyed  (sortKey)" $ nf (keyedSort coll) long
        ]
    ]
