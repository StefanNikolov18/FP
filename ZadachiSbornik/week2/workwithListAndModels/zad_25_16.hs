{-25.16. Да се дефинира функция mergeevenodd l1 l2, която получава два спи-
съка от цели числа l1 и l2 и връща списък, чиито елементи на четни
позиции са елементите на l1, а тези на нечетни позиции са елементите
на l2. Пример: mergeevenodd [1,2,3] [4,5,6] -> [1,4,2,5,3,6].-}

mergeevenodd :: [Int] -> [Int] -> [Int]
mergeevenodd [] l2 = l2
mergeevenodd l1 [] = l1
mergeevenodd (x : xs) (y : ys) = x : y : mergeevenodd xs ys
