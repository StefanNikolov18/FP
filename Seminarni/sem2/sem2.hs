--Recursion

--1)Напишете функция sum', която намира сумата на първите n естествени числа.
sum' :: Int -> Int
sum' 0 = 0
sum' n = n + sum' (n-1)

--2)Напишете функция countDigits, която намира броя на цифрите на естествено число.
countDigits :: Int -> Int
countDigits  n = countDigitsHelper (abs n)
    where 
        countDigitsHelper :: Int -> Int
        countDigitsHelper n 
            | n < 10 = 1
            | otherwise = 1 + countDigitsHelper (div n 10)


--3)Напишете функция divisorsSum, която намира сумата на всички делители на естествено число.
divisorSum :: Int -> Int
divisorSum n = divSumHelper n n
    where 
        divSumHelper :: Int -> Int -> Int
        divSumHelper 0 _ = 0
        divSumHelper k n 
            | n `mod` k == 0 = k + divSumHelper (k-1) n
            | otherwise = divSumHelper (k-1) n

--4)Напишете функция toBinary, която преобразува десетично число в двоично.
toBinary :: Int -> Int
toBinary 0 = 0
toBinary n = rem n 2 + 10 * toBinary(div n 2)

--6)Напишете функция fibonacci, която намира n-тото число на Фибоначи. Приемаме, че първото и второто са 1.
fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci  n = fibIter 2 1 1
    where
        fibIter :: Int -> Int -> Int -> Int
        fibIter k f1 f2
            | k == n = f1
            | otherwise = fibIter (k+1) (f1 + f2) f1


--7)Напишете функция palindrome, която проверява дали естествено число е палиндром.
palindrome :: Int -> Bool
palindrome n = abs n == reverse_ 0 (abs n)
    where
        reverse_ :: Int -> Int-> Int
        reverse_ r 0 = r
        reverse_ r n = reverse_ (r*10 + rem n 10) (div n 10)

--8)Напишете функция fastPow, приемаща рационално число x и цяло число n, 
--която намира xn, като използвате алгоритъма за бързо степенуване.
fastPow :: Double -> Int -> Double
fastPow _ 0 = 1
fastPow x n
  | even n    = half * half
  | otherwise = x * half * half
  where
    half = fastPow x (n `div` 2)


--9)Nапишете функция prime, която проверява дали естествено число е просто.
prime :: Int -> Bool
prime 1 = False
prime n = for 2 n
    where 
        for :: Int -> Int -> Bool
        for i n
            | fromIntegral i >= sqrt (fromIntegral n) = True
            | n `rem` i == 0 = False
            | otherwise = for (i + 1) n