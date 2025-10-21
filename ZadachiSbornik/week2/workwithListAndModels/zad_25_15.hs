{-25.15. За даден списък L, да се намерят елементите на списъка, чиято стой-
ност е по-голяма от сумата на предхождащите ги елементи. Пример
[1, 2, 5, 9, 16] → [1, 2, 5, 9].-}

greaterThanPrevSumElems ::[Int] -> [Int]
greaterThanPrevSumElems [] = []
greaterThanPrevSumElems (x : xs) = x : helperGTPSE xs x --pochvam ot vtori element
    where
        helperGTPSE :: [Int] -> Int -> [Int]
        helperGTPSE [] _ = []
        helperGTPSE (x : xs) sum
            | x > sum = x : helperGTPSE xs (sum + x)
            | otherwise = helperGTPSE xs sum 