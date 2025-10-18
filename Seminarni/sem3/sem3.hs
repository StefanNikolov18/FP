--Основни функции за работа със списъци
--1)Реализирайте ваши версии на следните вградени функции за работа със списъци: 
--length, elem, reverse, zip, concat, take, drop, (!!), (++)
import Prelude hiding (length, elem , reverse , zip ,concat , take , drop , (!!), (++))

length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys) = x == y || elem y ys

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x] 

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y: ys) = (x,y) : zip xs ys

concat :: [a] -> [a] -> [a]
concat l [] = l
concat l (x : xs) = concat (l ++ [x]) xs

take :: Int -> [a] -> [a]
take 0 _ = []
take n (x : xs) = x : take (n-1) xs


drop :: Int -> [a] -> [a]
drop 0 l = l
drop n (_ : xs)  = drop (n-1) xs

(!!) :: [a] -> Int -> a
(x : _) !! 0 = x
(_ : xs) !! n = xs !! (n - 1)


(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

--2)Напишете функция isSorted, която проверява дали списък е сортиран във възходящ ред.
isSorted :: Ord a => [a] -> Bool
isSorted [] = True 
isSorted [_] = True 
isSorted (x:y:xs) = x <= y && isSorted (y : xs)

--3)Напишете функция rotate, която приема списък и естествено число n.
-- Функцията да завърта списъка с n позиции на ляво.
rotate :: [a] -> Int -> [a]
rotate l n = drop n l ++ take n l

--4)Напишете функция removeEvery, която приема списък и естествено число n. 
--Функцията да премахва всеки n-ти елемент на списъка.
removeEvery :: [a] -> Int -> [a]
removeEvery [] _ = []
removeEvery l n = take (n-1) l ++ removeEvery (drop n l) n
 

--5)Напишете функция compress, която премахва от списък всички поредни повтарящи се елементи.
compress :: Eq a => [a] -> [a]
compress [] = []
compress l@[_] = l
compress (x : y : xs)
    | x == y = compress (x : xs)
    | otherwise = x : compress (y : xs)

--6)Напишете функция insert, която вмъква елемент на правилното му място в сортиран списък.
insert :: Ord a => [a] -> a -> [a]
insert [] y = [y]
insert (x : xs) y
    | x < y = x : insert xs y
    | otherwise = y : x : xs


--Напишете функция insertionSort, която сортира списък чрез алгоритъма insertion sort, която да ползва горната функция.
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x : xs) = insert (insertionSort xs) x 

--7)Напишете функция partition, която приема списък lst и елемент el от типа на елементите в списъка. 
--Функцията да връща наредена двойка от два списъка, като първият да съдържа елементите в lst, които са по-малки от el,
-- а вторият - по-големи или равни от el. Да се запази относителният ред на елементите.
partition :: Ord a => [a] -> a -> ([a],[a])
partition [] _ = ([],[])
partition (x : xs) y =
    let (less,greater) = partition xs y
    in if x < y then (x : less,greater) else (less, x : greater) 