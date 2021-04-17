{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveLift #-}
module Text.Collate.Trie
  ( Trie
  , empty
  , insert
  , alter
  , unfoldTrie
  , matchLongestPrefix
  )
  where

import qualified Data.IntMap as M
import Data.Bifunctor (first)
import Data.Binary (Binary(..))
import Language.Haskell.TH.Syntax (Lift(..))
import Instances.TH.Lift ()
import Data.Maybe (fromMaybe)
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup (Semigroup(..))
#endif
-- import Debug.Trace

data Trie a = Trie (Maybe a) (Maybe (M.IntMap (Trie a)))
  deriving (Show, Eq, Ord, Lift, Functor, Foldable, Traversable)

instance Semigroup (Trie a) where
   trie1 <> trie2 = foldr (uncurry insert) trie1 (unfoldTrie trie2)

instance Monoid (Trie a) where
   mempty = Trie Nothing Nothing
   mappend = (<>)

instance Binary a => Binary (Trie a) where
   put (Trie mbv mbm) = put (mbv, mbm)
   get = do
     (mbv,mbm) <- get
     return $ Trie mbv mbm

empty :: Trie a
empty = Trie Nothing Nothing

unfoldTrie :: Trie a -> [([Int], a)]
unfoldTrie  = map (first reverse) . go []
 where
  go xs (Trie (Just v) (Just m)) =
    (xs, v) : concatMap (gopair xs) (M.toList m)
  go xs (Trie (Just v) Nothing) = [(xs, v)]
  go xs (Trie Nothing (Just m)) =
    concatMap (gopair xs) (M.toList m)
  go _ (Trie Nothing Nothing) = []
  gopair xs (i, trie) = go (i:xs) trie

insert :: [Int] -> a -> Trie a -> Trie a
insert [] x (Trie _ mbm) = Trie (Just x) mbm
insert (c:cs) x (Trie mbv (Just m)) =
  case M.lookup c m of
    Nothing   -> Trie mbv (Just (M.insert c (insert cs x empty) m))
    Just trie -> Trie mbv (Just (M.insert c (insert cs x trie) m))
insert (c:cs) x (Trie mbv Nothing) =
  Trie mbv (Just (M.insert c (insert cs x empty) mempty))

alter :: (Maybe a -> Maybe a) -> [Int] -> Trie a -> Trie a
alter f [] (Trie mbv mbm) = Trie (f mbv) mbm
alter f (c:cs) (Trie mbv (Just m)) =
  Trie mbv (Just (M.insert c (alter f cs $ fromMaybe empty $ M.lookup c m) m))
alter f (c:cs) (Trie mbv Nothing) =
  Trie mbv (Just (M.insert c (alter f cs empty) mempty))

matchLongestPrefix :: Trie a -> [Int] -> Maybe (a, [Int], Trie a)
matchLongestPrefix = go Nothing
 where
   go _ (Trie (Just x) mbm) [] = Just (x, [], Trie Nothing mbm)
   go best (Trie Nothing  _) [] = best
   go best (Trie mbv mbm) (c:cs) =
     case mbm >>= M.lookup c of
       Nothing ->
         case mbv of
           Nothing -> best
           Just x  -> Just (x, c:cs, Trie Nothing mbm)
       Just trie -> go (case mbv of
                          Nothing -> best
                          Just x  -> Just (x, c:cs, trie)) trie cs

