{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveLift #-}
module UnicodeCollation.Trie
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
import Control.Applicative ((<|>))
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup (Semigroup(..))
#endif

data Trie a = Trie (Maybe a) (M.IntMap (Trie a))
  deriving (Show, Eq, Ord, Lift, Functor, Foldable, Traversable)

instance Semigroup (Trie a) where
   trie1 <> trie2 = foldr (uncurry insert) trie1 (unfoldTrie trie2)

instance Monoid (Trie a) where
   mempty = Trie Nothing mempty
   mappend = (<>)

instance Binary a => Binary (Trie a) where
   put (Trie mbv m) = put (mbv, m)
   get = do
     (mbv,m) <- get
     return $ Trie mbv m

empty :: Trie a
empty = Trie Nothing mempty

unfoldTrie :: Trie a -> [([Int], a)]
unfoldTrie  = map (first reverse) . go []
 where
  go xs (Trie (Just v) m) =
    (xs, v) : concatMap (gopair xs) (M.toList m)
  go xs (Trie Nothing m) =
    concatMap (gopair xs) (M.toList m)
  gopair xs (i, trie) = go (i:xs) trie

insert :: [Int] -> a -> Trie a -> Trie a
insert [] x (Trie _ m) = Trie (Just x) m
insert (c:cs) x (Trie mbv m) =
  case M.lookup c m of
    Nothing   -> Trie mbv (M.insert c (insert cs x empty) m)
    Just trie -> Trie mbv (M.insert c (insert cs x trie) m)

alter :: (Maybe a -> Maybe a) -> [Int] -> Trie a -> Trie a
alter f [] (Trie mbv m) = Trie (f mbv) m
alter f (c:cs) (Trie mbv m) =
  case M.lookup c m of
    Nothing   -> Trie mbv (M.insert c (alter f cs empty) m)
    Just trie -> Trie mbv (M.insert c (alter f cs trie) m)

matchLongestPrefix :: Trie a -> [Int] -> Maybe (a, [Int], Trie a)
matchLongestPrefix = go Nothing
 where
   go _ (Trie (Just x) m) [] = Just (x, [], Trie Nothing m)
   go best (Trie Nothing  _) [] = best
   go best (Trie mbv m) (c:cs) =
     case M.lookup c m of
       Nothing ->
         case mbv of
           Nothing -> best
           Just x  -> Just (x, c:cs, Trie Nothing m)
       Just trie -> go ((case mbv of
                           Nothing -> Nothing
                           Just x  -> Just (x, c:cs, trie)) <|> best) trie cs

