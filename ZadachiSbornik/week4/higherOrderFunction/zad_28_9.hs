{-28.9. (*) С помощта на foldr да се дефинира функция, която проверява да-
ли дали даден списък от числа l::[Int] е нареден във възходящ ред.
Упътване: Използвайте двойка (Bool,Int) за акумулатор.-}

l::[Int]
l = [1,2,3,4,5]

isSortedAscending :: [Int] -> Bool
isSortedAscending l = fst $ foldr opAsc (True,maxBound) l 
    where
        opAsc _ (False, b) = (False, b)
        opAsc a (x, b) 
            | a <= b    = (x, b)
            | otherwise = (False, a)