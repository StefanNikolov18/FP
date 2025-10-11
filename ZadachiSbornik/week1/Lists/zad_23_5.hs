
--23.5. Да се реализира функция common l1 l2, която преброява колко от еле-
--ментите на l1 са елементи и на l2.

common :: [Int] -> [Int] -> Int
common [] _ = 0
common (x : xs) l2
    | isElement x l2 == True = 1 + common xs l2
    | otherwise = common xs l2
        where
            isElement :: Int -> [Int] -> Bool
            isElement _ [] = False
            isElement x (x' : xs) = x == x' || isElement x xs