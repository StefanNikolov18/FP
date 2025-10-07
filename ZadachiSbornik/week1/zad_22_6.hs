--22.6. Да се дефинира функцията pow(x, k) = x^k
--за цели положителни числа x и k.

pow :: Int -> Int -> Int 
pow x k = if k == 0 then 1 else x * (pow x (k-1))

powBetter :: Int ->Int -> Int
powBetter x 0 = 1
powBetter x k 
    | even k  = powBetter (x * x) (k `div` 2)
    | otherwise  = x * powBetter x (k - 1)