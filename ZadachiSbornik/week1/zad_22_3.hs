-- a) check if number p is divisible by 4 or 7
func1 :: Int -> Bool
func1 p = p `mod` 4 == 0 || p `mod` 7 == 0

-- b) check ax^2 + bx + c = 0 has NOT real roots
func2 :: Int -> Int -> Int -> Bool
func2 a b c = b*b - 4 * a * c < 0

-- z) x is different than max[a,b,c]
func3 :: Int ->Int ->Int ->Int ->Bool
func3 x a b c = x /= maximum [a,b,c]

-- i) a b c are not positive
func4 :: Int -> Int -> Int ->Bool
func4 a b c = (a <= 0) && (b <= 0) && (c <= 0)

--k) digit 7 is in p (3 digit number) 
func5 :: Int ->Bool
func5 p = '7' `elem` show (abs p)
