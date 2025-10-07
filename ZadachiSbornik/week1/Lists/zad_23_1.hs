
--23.1. Да се реализира собствен вариант на функциите length, elem и sum за
--намиране на дължина на списък, проверка за принадлежност на елемент
--към списък и намиране на сумата на елементите на списък от числа.

lengthMy :: [Int] -> Int
lengthMy l = if null l then 0 else 1 + lengthMy(tail l)

elemMy :: [Int] -> Int -> Bool
elemMy l x = if null l then False else head l == x || elemMy (tail l) x

mySum :: [Int] -> Int
mySum l = if null l then 0 else head l + mySum(tail l)


