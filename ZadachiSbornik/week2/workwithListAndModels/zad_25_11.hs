{-25.11. Да се дефинира функция pack l :: [a] → [[a]], която получава списък и
връща списък от списъци, като всеки от тях съдържа всички последо-
вателни еднакви елементи на входния списък.-}

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack l = reverse $ packHelper l [] []
    where
        packHelper :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
        packHelper [] [] res = res
        packHelper [m] s res = (m : s) : res
        packHelper (x : y : xs) s res
            | x == y = packHelper (y:xs) (x:s) res
            | otherwise = packHelper (y:xs) [] ((x : s) : res)

