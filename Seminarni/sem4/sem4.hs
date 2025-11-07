{-Функции от по-висок ред-}

import Prelude hiding (const,curry,flip,map,filter,foldr,foldl,takeWhile,dropWhile,any,all,foldr1,foldl1)

{-Задача 01 - Основни функции от по-висок ред.
Реализирайте ваши версии на следните вградени функции от по-висок ред: const, curry, flip, (.).-}

const :: a -> (b -> a)
const x = \y -> x

curry :: ((a,b) -> c) -> a -> b -> c
curry f = \x y -> f(x,y)

flip :: (a -> b -> c) -> b -> a -> c
flip  f = \x y -> f y x

sub :: Int -> Int -> Int
sub x y = x - y

compose :: (b -> c) -> (a -> b) -> a -> c
compose g f x = g $ f x 


{-
2)Напишете функция on, която приема една двуместна функция g и една едноместна функция f. 
Резултатът на функцията да бъде нова двуместна функция h, която прилага f върху двата аргумента и
 след това прилага g върху двата резултата.
 -}

on :: (a -> a ->  b) -> (c -> a) -> c -> c -> b
on g f x y = g (f x) (f y)

{-
3)Напишете функция composeN, 
която приема функция f и естествено число n. 
Функцията да връща нова функция, която е n-кратната композоция на f.
-}

composeN :: (a -> a) -> Int -> (a -> a)
composeN f 0 = id
composeN f n = f . composeN f (n-1)

{-
4)Напишете функция liftB, която "повдига" функция, 
приемаща 2 булеви стойности и връщаща булева стойност, 
до функция приемаща 2 предиката и връщаща предикат. 
Напишете операторите (==>) и (<=>), реализиращи булевите функции за следствие и 
еквивалентност и с помощта на liftB напишете функциите implies и iff, които реализират същите функции, 
но за предикати.
-}

liftB :: (Bool -> Bool -> Bool) -> (a -> Bool) -> (a -> Bool) -> a -> Bool
liftB f p1 p2 x = f (p1 x) (p2 x) 

(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b

(<==>) :: Bool -> Bool -> Bool
a <==> b = a ==> b && b ==> a

implies :: (a -> Bool) -> (a -> Bool) -> a -> Bool
implies = liftB (==>)

iff :: (a -> Bool) -> (a -> Bool) -> a -> Bool
iff = liftB (<==>)

{-
5)Напишете функция derive, която приема функция, и връща нейната производна.
 Използвайте следната формула, като изберете достатъчно малко h:
-}

derive :: (Double -> Double) -> Double -> Double
derive f a =
    let h = 1e-6
    in (f(a + h) - f a )/h

{-
6)Реализирайте ваши версии на следните вградени функции 
от по-висок ред за работа със списъци: 
map, filter, foldr, foldl, takeWhile, dropWhile, zipWith, any, all, foldr1, foldl1.
-}

map :: (a -> a) -> [a] -> [a]
map _ [] = [] 
map f (x : xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs)
    | p x = x : filter p xs
    | otherwise = filter p xs


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ nv [] = nv
foldr op nv (x : xs) = op x $ foldr op nv xs 

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ nv [] = nv
foldl op nv (x : xs) = foldl op (op nv x) xs


takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile pred (x : xs)
    | pred x = x : takeWhile pred xs
    | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile   _ [] = []
dropWhile pred (x : xs)
    | pred x = dropWhile pred xs
    | otherwise = x : xs


any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any pred (x : xs) = pred x || any pred xs


all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all pred (x : xs) = pred x && all pred xs

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 op (x : xs) = foldr op x xs

foldl1 :: (a-> a -> a) -> [a] -> a
foldl1 op (x : xs) = foldl op x xs


{-
7)Напишете функция maximumBy, 
която намира максимален елемент в списък спрямо подадена функция за сравнение.
-}
maximumBy :: (a -> a -> Bool) -> [a] -> a
maximumBy cmp (x : xs) = foldl (\current result-> if cmp result current then current else result) x xs

{-
8)Напишете функция quickSortBy, 
която по подадена функция за сравнение сортира списък чрез алгоритъма quick sort.
-}

quickSortBy :: (a -> a -> Bool) -> [a] -> [a]
quickSortBy _ [] = []
quickSortBy cmp (x : xs) = 
    let pivot = x
        less = [y | y <- xs, y `cmp` pivot]
        greater = [y | y <- xs, not (y `cmp` pivot)]
    in quickSortBy cmp less ++ [pivot] ++ quickSortBy cmp greater


{-
9)Напишете функция groupBy, която приема функция на 1 аргумент f и списък.
 Функцията да групира всички елементи от списъка по стойността, която връща 
 функцията f приложена над тях. Резултата да е под формата на списък от наредени двойки,
 където първата компонента е стойността на f, а втората е списък от елементите, които дават тази стойност.
-}

groupBy :: (a -> a) -> [a] -> [(a,a)]
groupBy _ [] = []
groupBy f (x : xs) = (f x, x) : groupBy f xs

{-
10)Напишете функция subsets, която по подаден списък,
 връща всички подмножества от елементите на списъка.
-}
subset :: [a] -> [[a]]
subset [] = [[]]
subset (x : xs) = 
    let s = subset xs
    in map (x :) s ++ s
