--25.8. Да се дефинира функция groupsof l x, която разделя списъка l на гру-
--пи от по x елемента.
--Например, groupsof [1, 2, 3, 4, 5, 6, 7, 8] 3 → [[1, 2, 3], [4, 5, 6], [7, 8]].
groupsof :: [Int] -> Int -> [[Int]]
groupsof [] x = []
groupsof l x = groupsofHelper l x 0 [] []
    where
        groupsofHelper :: [Int] -> Int -> Int -> [Int] -> [[Int]] -> [[Int]]
        groupsofHelper [] _ _ partX res
            | null partX = reverse res
            | otherwise = reverse ((reverse partX) : res)
        groupsofHelper (y : ys) x i partX res
            | i == x = groupsofHelper (y:ys) x 0 [] ((reverse partX) : res)
            | otherwise = groupsofHelper ys x (i + 1) (y : partX) res