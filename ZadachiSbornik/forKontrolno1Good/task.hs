pow ::  (Fractional a, Eq a) => a -> Int -> a
pow n k
    | k < 0 = 1 / powN n (abs k)
    | k == 0 = 1
    | otherwise = powN n k
    where
        powN :: (Num a) => a -> Int -> a
        powN _ 0 = 1
        powN n k = n * powN n (k-1)


count :: Eq a =>  a ->[a] -> Int
count y [] = 0
count y (x : xs)
    | y == x = 1 + count y xs
    | otherwise = count y xs

sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _ = True
sublist (x : xs) list = x `elem` list && sublist xs list

duplicates :: Eq a => [a] -> Bool
duplicates []  = False
duplicates (x : xs) = x `elem` xs || duplicates xs

firstEven :: Integral a => [a] -> [a]
firstEven = takeWhile even  

allEven :: Integral a => [a] -> [a]
allEven = filter even

flatten :: [[a]] -> [a]
flatten = concat

decode :: [(Int,a)] -> [a]
decode [] = []
decode ((n,elem) : xs) = listCount elem (abs n) ++ decode xs
    where
        listCount :: a -> Int -> [a]
        listCount _ 0 = []
        listCount el n = el : listCount el (n-1) 


quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort _ [] = []
quickSort cmp (x : xs) = 
    let
        pivot = x
        lower = [x | x <- xs , x `cmp` pivot]
        higher = [y | y <- xs,not (y `cmp` pivot)]
    in quickSort cmp lower ++ [pivot] ++ quickSort cmp higher


mergeEvenOdd :: [a] -> [a] -> [a]
mergeEvenOdd l1 l2 =  concat (map (\(x,y) -> [x,y]) (zipWith (,) l1 l2))


type Triple = (Int,Int,Int) 

--a)
sumTriplets :: [Triple] -> [Int]
sumTriplets = map (\(x,y,z) -> x + y + z)

sumComponents :: [Triple] -> (Int,Int,Int)
sumComponents = foldr (\(x1,y1,z1) (x2,y2,z2) -> (x1+x2,y1+y2,z1+z2)) (0,0,0)

countTripletsIsTriangle :: [Triple] -> Int
countTripletsIsTriangle l = length $ filter (\(x,y,z) -> (x + y) > z) l

hasAllEquals :: [Triple] -> Bool
hasAllEquals = any (\(x,y,z) -> (x == y) && (y == z))


eqToIndex :: (Eq a,Enum a) => [a] -> [a]
eqToIndex list = map fst (filter (\(x,index) -> x == toEnum index) (zip list [1..]))

