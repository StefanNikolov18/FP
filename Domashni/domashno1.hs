{-Функционално програмиране – Домашно 1
    Stefan Nikolov
-}


--1) LDE2 - Linear Diophantine Equation

data LDE2 = LDE2{
    a :: Int,
    b :: Int,
    c :: Int
} deriving Show


--1.1) Solution to LDE2

concreteSolution :: LDE2 -> Maybe (Int,Int)
concreteSolution (LDE2 a b c) =
    let (d, u, v) = extendedGCD a b
    in if c `rem` d == 0 then Just (u * (c `div` d),v * (c `div` d)) else Nothing


extendedGCD :: Int  -> Int -> (Int,Int,Int)
extendedGCD a 0 = (a,1,0)
extendedGCD a b = 
    let (d, u, v) = extendedGCD b (a `mod` b)
    in (d,v,u - (a `div` b) * v)

checkSolution :: (Int,Int) -> LDE2 -> Bool
checkSolution (x,y) (LDE2 a b c) = a*x + b*y == c

{-test:
    concreteSolution (LDE2 1 4 3)
        Just (3,0)
    
    checkSolution (3,0) (LDE2 1 4 3)
        True
-}



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

{- test:
    take 10 $ diophantine (LDE2 1 4 3)
    [(3,0),(7,-1),(-1,1),(11,-2),(-5,2),(15,-3),(-9,3),(19,-4),(-13,4),(23,-5)]
-}



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

{-test:
    prettyPrint (LDE2 1 4 3)
        "1.x + 4.y = 3"

    prettyPrint (LDE2 9 (-3) 6)
        "9.x - 3.y = 6"
-}



--1.4) deserialization
        
toLDE2 :: String -> Maybe LDE2
toLDE2 s = 
    let 
        s' = dropWhile (== ' ') s
        aIsMinus = not (null s') && head s' == '-'
    in if aIsMinus 
        then splitEquation (tail s') >>= \parts ->
            parseTermsAndCheckRhs parts >>= \term ->
            splitSign term >>= \partsSign -> 
            validateTerms partsSign >>= (\res -> Just (updateA res))
        else
            splitEquation s' >>= \parts ->
            parseTermsAndCheckRhs parts >>= \term ->
            splitSign term >>= \partsSign -> 
            validateTerms partsSign
        where
            splitEquation :: String -> Maybe (String , String)
            splitEquation str
                | '=' `notElem` str = Nothing
                | otherwise = Just (takeWhile (/= '=') str,tail $ dropWhile (/= '=') str)

            parseTermsAndCheckRhs :: (String,String) -> Maybe (String,Int)      -- making rhs to word and check if is 1 elem and is digit                                                                   
            parseTermsAndCheckRhs (term,rhs) =                                  -- then parse it
                let inWords = words rhs
                in if length inWords == 1 && all (`elem` digits) (head inWords)
                        then Just (term, read (head inWords) :: Int) -- (term , c)
                        else Nothing

            digits :: String            -- set of digits and sign to check string if is a number 
            digits = "-0123456789"

            splitSign :: (String,Int) -> Maybe (String,String,Int,Bool)
            splitSign (str,c)
                | '+' `notElem` str && '-' `notElem` str = Nothing
                | otherwise = 
                     Just (takeWhile (\ch -> ch /= '+' && ch /= '-') str,
                      tail $ dropWhile (\ch -> ch /= '+' && ch /= '-') str,c,'-' `elem` str && '+' `notElem` str) -- (term1,term2,c,isMinus)
            
            validateTerms :: (String,String,Int,Bool) -> Maybe LDE2
            validateTerms (lhs,rhs,c,isMinus) = 
                let 
                    hasPointX = '.' `elem` lhs && 'x' `elem` tail (dropWhile (/= '.') lhs)
                    hasPointY = '.' `elem` rhs && 'y' `elem` tail (dropWhile (/= '.') rhs)
                in 
                    if hasPointX && hasPointY
                        then 
                            let 
                                a = takeWhile (/= '.') lhs 
                                b = takeWhile (/= '.') rhs
                            in 
                                if all (`elem` digits) (head (words a)) && all (`elem` digits) (head (words b))
                                    then 
                                        if isMinus 
                                            then Just (LDE2 (read (head (words a)) :: Int) (-(read (head (words b))) :: Int) c)
                                            else Just (LDE2 (read (head (words a)) :: Int) (read (head (words b)) :: Int) c)
                                    else Nothing
                        else Nothing

            updateA :: LDE2 -> LDE2
            updateA (LDE2 a b c) = LDE2 {a = -a, b = b, c = c}

{-test:
    toLDE2 "1.x + 4.y = 3"
        Just (LDE2 1 4 3)

    toLDE2 "4.x- 3.y =     8"
        Just (LDE2 4 (-3) 8)

    toLDE2 "4 - 3.y = 1"
        Nothing

    toLDE2 "-4.x-7.y=10"        
        Just (LDE2 (-4) (-7) 10)

    toLDE2 "3.x + y = 5"       
        Nothing 

    toLDE2 "4.x-3.y= -7"       
        Just (LDE2 4 (-3) (-7))
-}


--2) LDEN - Linear Diophantine Equation with N unknowns

data LDEn = LDEn{
    n :: Int,
    coefs :: [Int],
    rhs :: Int
} deriving Show


fixCoefs :: Int -> [(Int,Int)]
fixCoefs n = [(xi,xj) | xi <- [0..n-2], xj <- [xi + 1..n-1]]

combination :: Int -> [a] -> [[a]]
combination 0 _ = [[]]
combination _ [] = []
combination k (x:xs)
    | k < 0 = []
    | otherwise = map (x :) (combination (k-1) xs) ++ combination k xs

removeAtIndexes :: [Int] -> [Int] -> [Int]
removeAtIndexes [] list = list
removeAtIndexes indexes list = removeAtIndexesHelper indexes (zip list [0..])
    where
        removeAtIndexesHelper :: [Int] -> [(Int,Int)] -> [Int]
        removeAtIndexesHelper [] l = map fst l
        removeAtIndexesHelper (x : xs) l =
            let filtered = filter (\(_,index) -> index /= x) l
            in  removeAtIndexesHelper xs filtered
    

generateLDE2FromLDEn :: [Int] -> LDEn -> [LDE2]
generateLDE2FromLDEn ys (LDEn n coefs rhs) = 
       [LDE2 {a = coefs !! xi ,b = coefs !! xj, c = remainder} | 
        (xi,xj) <- fixCoefs n,
        getRemainders <- combination (n - 2) ys,
        remainder <- [ sum (rhs :map (*(-1)) (zipWith (*) getRemainders (removeAtIndexes [xi,xj] coefs)))]
    ]
    
                
{-test:
    generateLDE2FromLDEn [1,2] (LDEn { n = 3, coefs = [1,4,1], rhs = 3 })
        [LDE2 {a = 1, b = 4, c = 2},LDE2 {a = 1, b = 4, c = 1},LDE2 {a = 1, b = 1, c = -1},
        LDE2 {a = 1, b = 1, c = -5},LDE2 {a = 4, b = 1, c = 2},LDE2 {a = 4, b = 1, c = 1}]

-}