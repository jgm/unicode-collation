{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module UnicodeCollation.Elements
  ( getCollationElements
  , parseCollation
  , canonicalCombiningClass
  )
where

import UnicodeCollation.Types

import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as M
import Data.Bits ( Bits((.|.), shiftR, (.&.)) )
import Data.ByteString.Lex.Integral (readHexadecimal)
import Data.List (foldl', permutations, sortBy)
import Data.Ord
import qualified Data.Binary as Binary
import UnicodeCollation.CombiningClass (genCombiningClassMap)
import Data.Maybe
-- import Debug.Trace

-- S2.1 Find the longest initial substring S at each point that
-- has a match in the collation element table.
--
--     S2.1.1 If there are any non-starters following S, process each
--     non-starter C.
--
--     S2.1.2 If C is an unblocked non-starter with respect to S,
--     find if S + C has a match in the collation element table.
--
--     S2.1.3 If there is a match, replace S by S + C, and remove C.
--
-- Blocking Context: The presence of a character B between two characters
-- C1 and C2, where ccc(B) = 0 or ccc(B) ≥ ccc(C2).
--
-- Non-Starter: An assigned character with Canonical_Combining_Class ≠ 0.
--
-- Unblocked Non-Starter: A non-starter C2 which is not in a blocking
-- context with respect to a preceding character C1 in a string.
--
-- In the context <C1 ... B ... C2>, if there is no intervening
-- character B which meets the criterion for being a blocking context,
-- and if C2 is a non-starter, then it is also an unblocked non-starter.

-- | Retrieve the collation elements defined by a collation for
-- a sequence of code points.  These are used to construct a 'SortKey'.
getCollationElements :: Collation -> [Int] -> [CollationElement]
getCollationElements collation = go
 where
  matcher = matchLongestPrefix collation
  go [] = []
  go (c:cs) = case matcher (c:cs) of
                Nothing -> calculateImplicitWeight c ++ go cs
                Just (elts, [], _) -> elts
                Just (elts, is, subcollation)
                 | null unblockedNonStarters -> elts ++ go is
                 | otherwise ->
                     case sortBy (comparing remainderLength) matches of
                       ((newelts, rs, _):_)
                              -> newelts ++ go (rs ++
                                         drop (length unblockedNonStarters) is)
                       []   -> elts ++ go is
                  -- Now we need to check the whole sequence of
                  -- unblocked nonstarters, which can come in different orders
                    where
                      getUnblockedNonStarters _ [] = []
                      getUnblockedNonStarters n (x:xs)
                        = let ccc = canonicalCombiningClass x
                           in if ccc > n
                                 then x : getUnblockedNonStarters ccc xs
                                 else []
                      unblockedNonStarters = getUnblockedNonStarters 0 is
                      matches = mapMaybe (matchLongestPrefix subcollation)
                                 (take 24 (permutations unblockedNonStarters))
                      remainderLength (_,ys,_) = length ys

-- see 10.1.3, Implicit Weights
-- from allkeys.txt:
-- @implicitweights 17000..18AFF; FB00 # Tangut and Tangut Components
-- @implicitweights 18D00..18D8F; FB00 # Tangut Supplement
-- @implicitweights 1B170..1B2FF; FB01 # Nushu
-- @implicitweights 18B00..18CFF; FB02 # Khitan Small Script
calculateImplicitWeight :: Int -> [CollationElement]
calculateImplicitWeight cp =
  [CollationElement False (fromIntegral aaaa) 0x0020 0x0002 0xFFFF,
   CollationElement False (fromIntegral bbbb) 0 0 0xFFFF]
 where
  range x y = IntSet.fromList [x..y]
  singleton = IntSet.singleton
  union = IntSet.union
  -- from PropList.txt in unicode data:
  unifiedIdeographs =    range 0x3400 0x4DBF `union`
                         range 0x4E00 0x9FFC `union`
                         range 0xFA0E 0xFA0F `union`
                         singleton 0xFA11 `union`
                         range 0xFA13 0xFA14 `union`
                         singleton 0xFA1F `union`
                         singleton 0xFA21 `union`
                         range 0xFA23 0xFA24 `union`
                         range 0xFA27 0xFA29 `union`
                         range 0x20000 0x2A6DD `union`
                         range 0x2A700 0x2B734 `union`
                         range 0x2B740 0x2B81D `union`
                         range 0x2B820 0x2CEA1 `union`
                         range 0x2CEB0 0x2EBE0 `union`
                         range 0x2CEB0 0x2EBE0 `union`
                         range 0x30000 0x3134A
  -- from Blocks.txt in unicode data:
  cjkCompatibilityIdeographs = range 0xF900 0xFAFF
  cjkUnifiedIdeographs = range 0x4E00 0x9FFF
  (aaaa, bbbb) =
    case cp of
    _ | cp >= 0x17000 , cp <= 0x18AFF -- Tangut and Tangut Components
        -> (0xFB00, (cp - 0x17000) .|. 0x8000)
      | cp >= 0x18D00 , cp <= 0x18D8F -- Tangut Supplement
        -> (0xFB00, (cp - 0x17000) .|. 0x8000)
      | cp >= 0x1B170 , cp <= 0x1B2FF -- Nushu
        -> (0xFB01, (cp - 0x1B170) .|. 0x8000)
      | cp >= 0x18B00 , cp <= 0x18CFF -- Khitan Small Script
        -> (0xFB02, (cp - 0x18B00) .|. 0x8000)
      | cp `IntSet.member` unifiedIdeographs &&
        (cp `IntSet.member` cjkUnifiedIdeographs ||
         cp `IntSet.member` cjkCompatibilityIdeographs)  -- Core Han
        -> (0xFB40 + (cp `shiftR` 15), (cp .&. 0x7FFF) .|. 0x8000)
      | cp `IntSet.member` unifiedIdeographs -- All Other Han Unified ?
        -> (0xFB80 + (cp `shiftR` 15), (cp .&. 0x7FFF) .|. 0x8000)
      | otherwise
        -> (0xFBC0 + (cp `shiftR` 15), (cp .&. 0x7FFFF) .|. 0x8000)

readCodepoints :: B.ByteString -> ([Int], B.ByteString)
readCodepoints b =
  case readHexadecimal b of
    Nothing -> ([], b)
    Just (codepoint, rest) ->
      let (cps, b') = readCodepoints (B.dropWhile (==' ') rest)
        in (codepoint:cps, b')

-- | Parse a 'Collation' from a 'ByteString' in the format of
-- @allkeys.txt@.
parseCollation :: B.ByteString -> Collation
parseCollation = foldl' processLine mempty . B.lines
 where
  processLine trie b =
    case readCodepoints b of
      ([],_) -> trie
      (c:cs, rest) -> insertElements (c:cs) (go rest) trie
  go b =
    case B.break (== ']') (B.drop 1 $ B.dropWhile (/= '[') b) of
      (contents, rest)
         | B.null rest -> []
         | otherwise   -> parseContents contents : go rest
  parseContents b =
    let isVariable = not (B.null b) && B.head b == '*'
        isIgnorable (0,0,0) = True
        isIgnorable _       = False
    in case map readHexadecimal $ filter (not . B.null)
                                  (B.splitWith isSep b) of
              [Just (x,_), Just (y,_), Just (z,_)]
                -> CollationElement isVariable x y z
                                    (if isVariable || isIgnorable (x,y,z)
                                        then 0
                                        else 0xFFFF)
              _ -> CollationElement isVariable 0 0 0 0
  isSep '*' = True
  isSep '.' = True
  isSep _   = False


combiningClassMap :: M.IntMap Int
combiningClassMap = Binary.decode
   $(genCombiningClassMap "data/DerivedCombiningClass.txt")

-- | Determine the canonical combining class for a code point.
canonicalCombiningClass :: Int -> Int
canonicalCombiningClass cp = fromMaybe 0 $ M.lookup cp combiningClassMap
