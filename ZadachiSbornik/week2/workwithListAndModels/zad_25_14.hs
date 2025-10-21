{-25.14. Да се дефинира функция removeDuplicates l, която премахва всички
повторения на елементите на списъка l.-}

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates l = removeDuplicatesHelper l []
    where
        removeDuplicatesHelper :: Eq a => [a] -> [a] -> [a]
        removeDuplicatesHelper [] _ = []
        removeDuplicatesHelper (x : xs) seen
            | x `elem` seen = removeDuplicatesHelper xs seen
            | otherwise = x : removeDuplicatesHelper xs (x : seen)
