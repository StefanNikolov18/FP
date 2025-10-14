--25.9. Да се дефинира функция flattenl :: [[a]] → [a], която получава списък от
--списъци и връща списък, който съдържа всички елементи на входните
--списъци.

flattenl :: [[Int]] -> [Int]
flattenl [] = []
flattenl l = flattenlHelper l []
    where
        flattenlHelper :: [[Int]] -> [Int] -> [Int]
        flattenlHelper [] res = res
        flattenlHelper (x : xs) res = flattenlHelper xs (res ++ x)
