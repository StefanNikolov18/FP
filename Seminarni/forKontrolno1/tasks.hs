
sum' :: Int -> Int
sum' 0 = 0
sum' n = n + sum' (n - 1)

countDigits :: Int -> Int
countDigits n
    | n < 10 && n > (-10) = 1
    | otherwise =  1 + countDigits (n `div` 10)

divisorSum :: Int -> Int
divisorSum n = foldr (+) 0 (listDivisors 1 n)
    where
        listDivisors :: Int -> Int -> [Int]
        listDivisors i n
            | i == n = [n]
            | n `rem` i == 0 = i : listDivisors (i+1) n
            | otherwise = listDivisors (i + 1) n

toBinary :: Int -> Int
toBinary 0 = 0
toBinary n = rem n 2 + 10 * toBinary(div n 2)

fibonacci :: Int -> Int
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacciIter 2 1 1
    where
        fibonacciIter :: Int -> Int -> Int -> Int
        fibonacciIter k f1 f2
            | k == n = f1
            | otherwise = fibonacciIter (k + 1) (f1 + f2) f1


palindrome :: Int -> Bool
palindrome n = abs n == reverse' 0 n
    where
        reverse' :: Int -> Int -> Int
        reverse' res 0 = res
        reverse' res n = reverse' (n `rem` 10 + res * 10) (n `div` 10)


--Lists
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : xs) = x <= y && isSorted (y : xs)

rotate :: [a] -> Int -> [a]
rotate l 0 = l
rotate (x : xs) n = rotate (xs ++ [x]) (n-1) 


removeEvery :: [a] -> Int -> [a]
removeEvery l n = map fst (filter (\(_,i) -> i `rem` n /= 0) (zip l [1 ..]))

         

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y    = compress (y:xs)
    | otherwise = x : compress (y:xs)

partition :: Ord a => [a] -> a -> ([a],[a])
partition lst elem =
    let 
        lower = [x | x<- lst,x < elem]
        higher= [y | y <- lst , y >= elem]
    in (lower,higher)


merge :: Ord a => [a] -> [a] -> [a]
merge [] l2 = l2
merge l1 [] = l1 
merge (x : xs) (y : ys)
    | x <= y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

hasUniqueElements :: Eq a => [a] -> Bool
hasUniqueElements [] = True
hasUniqueElements [_] = True
hasUniqueElements (x : xs) = not (x `elem` xs) && hasUniqueElements xs 

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf (x : xs) l2 = x `elem` l2 && isInfixOf xs l2   

--functionHigherOrder
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c 
on g f x y = g (f x) (f y)



composeN :: (a -> a) -> Int -> (a -> a)
composeN _ 0 = \x -> x
composeN f n = f . composeN f (n-1)

maximumBy :: (a -> a -> Bool) -> [a] -> a
maximumBy cmp (x : xs) = foldl (\current result -> 
    if current `cmp` result then current else result) x xs

quickSortBy :: (a -> a -> Bool) -> [a] -> [a]
quickSortBy _ [] = []
quickSortBy _ l@[_] = l 
quickSortBy cmp (x : xs) =
    let 
        pivot = x
        lowerThanPivot = [x | x <- xs , x `cmp` pivot]
        higherThanPivot = [y | y <- xs, not $ y `cmp` pivot]
    in 
        quickSortBy cmp lowerThanPivot ++ [pivot] ++ quickSortBy cmp higherThanPivot


removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

groupBy :: Eq b => (a -> b) -> [a] -> [(b,[a])]
groupBy _ [] = []
groupBy f l =
    let fl = removeDuplicates $ map f l
    in creategroups fl f l
    where
        creategroups :: Eq b => [b] -> (a -> b) -> [a] -> [(b,[a])]
        creategroups [] _ _ = []
        creategroups (x : xs) f list =
            (x , filter (\y -> f y == x) list) : creategroups xs f list


subset :: [a] -> [[a]]
subset [] = []
subset (x : xs) = 
    let s = subset xs
    in (map (x :) s) ++ s

rationals :: [(Int,Int)]
rationals = [(x,y)| n <- [0..] , x <- [0 .. n],y <- [0..n], x + y == n]

pythagoreanTriples :: [(Int,Int,Int)]
pythagoreanTriples = [(x, y, z) | x <- [1..], y <- [x..x + 10], z <- [y..y + 10], x*x + y*y == z*z]


generateSumList :: Int -> Int -> [[Int]]
generateSumList k s = [x | x <- sequence (replicate k [0 .. s]) ,  sum x == s] 

permutation :: [Int] -> [[Int]]
permutation [] = [[]]
permutation list=  []