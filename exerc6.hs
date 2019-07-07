{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data
import Data.Typeable

data T a = T a [T a]
      deriving (Eq, Ord, Show, Data, Typeable)

someTree :: Int -> Int -> a -> T a
someTree a 0 v = T v []
someTree a h v = T v $ take a $ repeat $ someTree a (h-1) v

size :: T a -> Int
size (T a []) = 1
size (T a l)  = (1+) $ sum $ map size l

height :: T a -> Int
height (T a []) = 0
height (T a l)  = (+1) $ maximum $ map height l

mirror :: T a -> T a
mirror (T a []) = T a []
mirror (T a l)  = T a $ reverse (map mirror l)

arity :: T a -> Int
arity (T a []) = 1
arity (T a l)  = maximum $ [length l] ++ (map arity l)
