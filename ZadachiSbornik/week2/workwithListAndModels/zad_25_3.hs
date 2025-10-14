--25.3. Да се дефинира функция countEvenOddl, която за списъка от цели числа
--l връща наредена двойка от броя на четните и броя на нечетните числа
--в l.

type PairEvenOddCnt = (Int,Int)

countEvenOddl :: [Int] -> PairEvenOddCnt
countEvenOddl [] = (0,0)
countEvenOddl l = countEvenOddlHelper (0,0) l
    where
        countEvenOddlHelper :: PairEvenOddCnt -> [Int] -> PairEvenOddCnt
        countEvenOddlHelper p [] = p
        countEvenOddlHelper (i,j) (x : xs)
            | even x = countEvenOddlHelper (succ i,j) xs
            | otherwise = countEvenOddlHelper (i,succ j) xs
