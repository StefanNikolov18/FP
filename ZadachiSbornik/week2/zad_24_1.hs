--24.1. Да се съставят следните списъци:
--а) Първите n четни числа;
evenNList :: Int -> [Int]
evenNList 0 = []
evenNList n
    |  even n = n : evenNList (n-1)
    | otherwise = evenNList (n-1)

--б) Първите n члена на аритметична прогресия с първи член a и раз-
--лика d;

--в) [1!, 2!, ..., n!] за дадено n;
factNList :: Int -> [Int]
factNList 0 = []
factNList n = factNList (n-1) ++ [fact n]
    where
        fact :: Int -> Int
        fact 0 = 1
        fact n = n * fact(n-1)

--г) Всички четни числа;

--д) Всички членове на аритметична прогресия с първи член a и разли-
--ка d;

--е) [1!, 2!, ...] (безкраен списък)


