{-25.12. Да се дефинира функция encode l :: [a] → [(Int, a)], която получава спи-
сък и връща списък от двойки, където първият елемент на двойката е

броят на последователните еднакви елементи от входния списък, а вто-
рият елемент е самият елемент.-}

encode :: Eq a => [a] -> [(Int,a)]
encode [] = []
encode l = encodeHelper l 0
    where
        encodeHelper :: Eq a => [a] -> Int -> [(Int,a)]
        encodeHelper [] _ = []
        encodeHelper [x] cnt = [(cnt + 1,x)]
        encodeHelper (x : y : xs) cnt
            | x == y = encodeHelper (y:xs) (cnt + 1)
            | otherwise = (cnt+1,x) : encodeHelper (y:xs) 0
