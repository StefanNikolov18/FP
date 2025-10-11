--23.4. Да се реализира функция sublist l1 l2, която проверява дали всички
--елементи на l1 са елементи и на l2.

sublist :: [Int] -> [Int] -> Bool
sublist [] _ = True
sublist (x : xs) l2 = isElem x l2 && sublist xs l2
    where 
        isElem :: Int -> [Int] -> Bool
        isElem _ [] = False
        isElem x (x' : xs) = x == x' || isElem x xs