{-25.13. Да се дефинира функция remove x l, която премахва (а) първото сре-
щане на елемента x от списъка l и (б) всички срещания на елемента x
от списъка l.-}
--a)
removeFirstSeen :: Eq a => a -> [a] -> [a]
removeFirstSeen _ [] = []
removeFirstSeen x' (x'' : xs)
    | x' == x'' = xs
    | otherwise = x'' : removeFirstSeen x' xs

removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []
removeAll x (x' : xs)
    | x == x' = removeAll x xs
    | otherwise = x' : removeAll x xs    