-- 24.5. Да се дефинира функция histogram, която по символен низ s връща
--списък от двойки (ci,ni), където ci са различните символи от s, а ni
--е броя на срещания на ci в s. Например, histogram ”abracadabra” →
--[(a, 5), (b, 2), (r, 2),(c,1),(d,1)]. Използвайте помощни функции.

type Pair = ( Char , Int )

charsCnt :: [Int]
charsCnt = replicate 26 0

incrementAt :: Int -> [Int] -> [Int]
incrementAt idx lst = take idx lst ++ [lst !! idx + 1] ++ drop (idx + 1) lst

histogram :: String -> [Pair]
histogram [] = []
histogram str = listCntChars 'a' (histogramHelper charsCnt str)
    where
        histogramHelper :: [Int] -> String -> [Int]
        histogramHelper cntChars [] = cntChars
        histogramHelper cntChars (x : xs) = 
            histogramHelper (incrementAt (fromEnum x - fromEnum 'a') cntChars) xs
    
        listCntChars :: Char -> [Int] ->[Pair]
        listCntChars _ [] = []
        listCntChars ch (x : xs)
            | x == 0 = listCntChars (succ ch) xs
            | otherwise = ( ch , x ) : listCntChars (succ ch) xs
