--24.1. Да се съставят следните списъци:
--а) Първите n четни числа;
evenNList :: Int -> [Int]
evenNList 0 = []
evenNList n
    |  even n = n : evenNList (n-1)
    | otherwise = evenNList (n-1)

--б) Първите n члена на аритметична прогресия с първи член a и раз-
--лика d;
arithmeticProgression :: Int -> Int -> Int -> [Int]
arithmeticProgression _ _ 0 = []
arithmeticProgression a d n = a : arithmeticProgression (a + d) d (n-1)


--в) [1!, 2!, ..., n!] за дадено n;
factNList :: Int -> [Int]
factNList 0 = []
factNList n = factNList (n-1) ++ [fact n]
    where
        fact :: Int -> Int
        fact 0 = 1
        fact n = n * fact(n-1)

--г) Всички четни числа;
allEven :: [Int]
allEven = allEvenIter 0
    where
        allEvenIter :: Int -> [Int]
        allEvenIter i
            | even i = i : allEvenIter(i+1)
            | otherwise = allEvenIter(i+1)

--д) Всички членове на аритметична прогресия с първи член a и разли-
--ка d;
allArithmeticProgression :: Int -> Int -> [Int]
allArithmeticProgression a d = a : allArithmeticProgression (a+d) d

--е) [1!, 2!, ...] (безкраен списък)
factListInfinity :: [Int]
factListInfinity = factListInfinityIter 1
    where
        factListInfinityIter :: Int -> [Int]
        factListInfinityIter i = factorial i : factListInfinityIter (i + 1)
            where
                factorial :: Int -> Int
                factorial 0 = 1
                factorial n = n * factorial(n-1)



