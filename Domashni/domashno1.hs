{-Функционално програмиране – Домашно 1
    Stefan Nikolov
-}


--1) LDE2 - Linear Diophantine Equation

data LDE2 = LDE2{
    a :: Int,
    b :: Int,
    c :: Int
} deriving Show

hasSolution :: LDE2 -> Bool
hasSolution (LDE2 a b c) = c `rem` gcd a b == 0

--1.1)
concreteSolution :: LDE2 -> Maybe (Int,Int)
concreteSolution (LDE2 a b c)
    | not $ hasSolution (LDE2 a b c) = Nothing
    | otherwise = Just $ findSolution (LDE2 a b c) [0..]
        where
            findSolution :: LDE2 -> [Int] -> (Int,Int)
            findSolution (LDE2 a b c) (x : xs) = 
                let y = (c - a * x) `div` b
                in if checkSolution (x,y) (LDE2 a b c) then (x,y) else findSolution (LDE2 a b c) xs

checkSolution :: (Int,Int) -> LDE2 -> Bool
checkSolution (x,y) (LDE2 a b c) = a*x + b*y == c

--1.2)
diophantine :: LDE2 -> [(Int,Int)]
diophantine (LDE2 a b c) = 
    let 
        Just (x0, y0) = concreteSolution (LDE2 a b c)
        d = gcd a b
        u = b `div` d
        v = a `div` d
    in  (x0,y0) : [(x0 + k * u , y0 - k * v) | k <- kSequence]
        where
            kSequence :: [Int]
            kSequence = concat [[n,-n] | n <- [1..]]

--1.3) formating
prettyPrint :: LDE2 -> String
prettyPrint (LDE2 a b c) =
    let
        sign = getSign b
        num2 = parseSecondArgument b
    in
        show a ++ ".x " ++ sign ++ " " ++ num2 ++ ".y = " ++ show c
        where
            getSign :: Int -> String
            getSign x
                | x < 0 = "-"
                | otherwise = "+"

            parseSecondArgument :: Int -> String
            parseSecondArgument x
                | x < 0 = show $ abs x
                | otherwise = show x


--1.4) deserialization

toLDE2 :: String -> Maybe LDE2
toLDE2 str
    | not $ isValidLDE2String str = Nothing
    | otherwise = Just $ parseLDE2 str


isValidLDE2String :: String -> Bool
isValidLDE2String str = 
    '=' `elem` str && 
    isValidTerm (words left) && 
    isValidAfterEqual  (words right)
        where
            left = takeWhile (/= '=') str
            right = tail $ dropWhile (/= '=') str
            
            isValidTerm :: [String] -> Bool
            isValidTerm [term1, op, term2] = 
                isValidFactor term1 && (op == "+" || op == "-") && isValidFactor term2
            isValidTerm _ = False
                
            isValidFactor :: String -> Bool
            isValidFactor term =
                case break (== '.') term of
                    (coef, '.': var) -> all isDigit coef && var `elem` ["x","y"]
                    _ -> False

            isValidAfterEqual :: [String] -> Bool
            isValidAfterEqual [num] = all isDigit num
            isValidAfterEqual _ = False

            isDigit :: Char -> Bool
            isDigit x = x >= '1' && x <= '9'


parseLDE2 :: String -> LDE2
parseLDE2 str =
    let ws = words str                 -- ["3.x","+","4.y","=","5"]
        l = head ws                    -- "3.x"
        r = ws !! 2                    -- "4.y"
        res = ws !! 4                  -- "5"
    in LDE2
        { a = toDigit l
        , b = toDigit r
        , c = read res                 
        }
  where
    toDigit :: String -> Int
    toDigit s = read (takeWhile (/= '.') s)
           
                
                
