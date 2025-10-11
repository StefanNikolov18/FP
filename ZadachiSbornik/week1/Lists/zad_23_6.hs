--23.6. Да се реализира функция duplicates l, която проверява дали в списъка
--l има повтарящи се елементи.

duplicates :: [Int] -> Bool
duplicates [] = False
duplicates (x : xs) = isElem x xs || duplicates xs
    where
        isElem :: Int -> [Int] -> Bool
        isElem _ [] = False
        isElem x (x' : xs) = x == x' || isElem x xs