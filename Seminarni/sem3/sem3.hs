--Основни функции за работа със списъци
--1)Реализирайте ваши версии на следните вградени функции за работа със списъци: 
--length, elem, reverse, zip, concat, take, drop, (!!), (++)
import Prelude hiding (length, elem , reverse , zip ,concat , take , drop , (!!), (++))

length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys) = x == y || elem x ys

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

--8)Напишете функция merge, която приема два сортирани списъка и връща нов сортирант списък, 
--съдържащ елементите на двата списъка.    
merge ::Ord a => [a] -> [a] -> [a]
merge [] l = l
merge l [] = l
merge (x : xs) (y : ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


--9)Напишете функция nub, която премахва всички повтарящи се елементи в списък.
-- Редът на елементите в резултата няма значение.
nub :: Eq a => [a] -> [a]
nub [] = []
nub l  = nubHelper l []
    where
        nubHelper :: Eq a => [a] -> [a] -> [a]
        nubHelper [] _  = []
        nubHelper (x : xs) seen
            | x `elem` seen = nubHelper xs seen
            | otherwise = x : nubHelper xs (x : seen)

--10) Напишете функция isInfixOf, която проверява дали един списък е подсписък на друг.
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf l1 l2@(_ : xs) = isPref l1 l2 || isInfixOf l1 xs
    where
        isPref :: Eq a => [a] -> [a] -> Bool
        isPref [] _  = True
        isPref _ [] = False
        isPref (x : xs) (y : ys) = x == y && isPref xs ys
