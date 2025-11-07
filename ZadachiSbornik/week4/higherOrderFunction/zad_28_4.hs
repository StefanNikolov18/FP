{-28.4. За списък от числа L да се намери списък със сумите на всички двойки
последователни елементи на L. Например [1, 5, 3, 4, 2] → [6, 8, 7, 6]. Упът-
ване: Използвайте zip.-}

sumInZip :: Num a => [(a,a)] -> [a]
sumInZip [] = []
sumInZip ((x,y) : xs) = (x + y) : sumInZip xs

sumPairs :: Num a => [a] -> [a]
sumPairs [] = []
sumPairs l@[_] = l 
sumPairs list = sumInZip $ zip (take (length list - 1) list) (drop 1 list) 

