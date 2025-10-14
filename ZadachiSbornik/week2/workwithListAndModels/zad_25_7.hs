--25.7. Да се дефинира функция, която по даден списък от цели числа l връща
--списък от всички двойки (a, b) от l, за които a и b са съседни елементи
--в l и a < b.

type Pair = (Int, Int)

ascendingPairsInList :: [Int] -> [Pair]
ascendingPairsInList [] = []
ascendingPairsInList l = ascPairHelper [] l
    where
        ascPairHelper :: [Pair] -> [Int] -> [Pair]
        ascPairHelper res [x] = reverse res
        ascPairHelper res (a : b : xs)
            | a < b = ascPairHelper ((a,b) : res) (b : xs)
            | otherwise = ascPairHelper res (b:xs)
