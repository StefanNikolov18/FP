{-28.1. Направете и тествайте собствена реализация на функциите map, filter
и fold(l,r,l1,r1).-}

import Prelude hiding (map,filter,foldl,foldr,foldl1,foldr1)
import Data.List hiding (map,filter,foldl,foldr,foldl1,foldr1)

map :: (a -> a) -> [a] -> [a]
map _ [] = []
map f (x : xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter pred (x : xs)
    | pred x = x : filter pred xs
    | otherwise = filter pred xs

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ nv [] = nv
foldl op nv (x : xs) = foldl op (op nv x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr _ nv [] = nv
foldr op nv (x : xs) = op x $ foldr op nv xs 

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 op (x : xs) = foldl op x xs

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 op (x : xs) = foldr op x xs 
