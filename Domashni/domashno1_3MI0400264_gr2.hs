{-Функционално програмиране – Домашно 1
    Stefan Nikolov
-}


--1) LDE2 - Linear Diophantine Equation

data LDE2 = LDE2{
    a :: Int,
    b :: Int,
    c :: Int
}

instance Show LDE2 where
    show :: LDE2 -> String
    show (LDE2 a b c) = "(LDE2 " ++ show a ++ " " ++ show b ++ " " ++ show c ++ ")"

--1.1) Solution to LDE2
--a)
concreteSolution :: LDE2 -> Maybe (Int,Int)
concreteSolution (LDE2 a b c) =
    let (d, u, v) = extendedGCD a b
    in if c `rem` d == 0 then Just (u * (c `div` d),v * (c `div` d)) else Nothing


extendedGCD :: Int  -> Int -> (Int,Int,Int)
extendedGCD a 0 = (a,1,0)
extendedGCD a b = 
    let (d, u, v) = extendedGCD b (a `mod` b)
    in (d,v,u - (a `div` b) * v)


--b)
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
            kSequence = concat [[n,-n] | n <- [1..]]            -- kSequence = [1,-1,2,-2,3,-3..]

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
{-          '='      -}
{-        /     \    -}
{-      +/-       c  -}
{-      / \          -}
{-    .x   .y        -}
{-    /      \       -}
{-   a         b     -}
toLDE2 :: String -> Maybe LDE2                                            
toLDE2 s =  do                                                             
    let s' = dropWhile (== ' ') s                         -- take spaces upfront         
        aIsMinus = not (null s') && head s' == '-'        -- check if a is with minus
        input = if aIsMinus then tail s' else s'          -- cut the minus

    partsEq <- splitEquation input                           -- "a.x + b.y" = term , c = rhs
    terms <- parseTermsAndCheckRhs partsEq
    partsSign <- splitSign terms
    coefsTerms <- validateTermsHasPointXAndPointYAndPassCoef partsSign
    res <- validateTermsAreNumbers coefsTerms
    
    return $ if aIsMinus then negateA res else res
        
        where
            digits :: String            -- set of digits and sign to check string if is a number 
            digits = "-0123456789"

            negateA :: LDE2 -> LDE2
            negateA (LDE2 a b c) = LDE2 {a = -a, b = b, c = c}

            splitEquation :: String -> Maybe (String , String)                           -- "a.x + b.y = c" -> ("a.x + b.y", "c") 
            splitEquation str
                | '=' `notElem` str = Nothing
                | otherwise = Just (takeWhile (/= '=') str,tail $ dropWhile (/= '=') str)

            parseTermsAndCheckRhs :: (String,String) -> Maybe (String,Int)      -- ("a.x + b.y", "c") -> ("a.x + b.y", c)                                                                
            parseTermsAndCheckRhs (term,rhs) =                                  -- making rhs to word and check if is 1 elem and is digit
                let inWords = words rhs                                         -- then parse it
                in if length inWords == 1 && all (`elem` digits) (head inWords)
                        then Just (term, read (head inWords) :: Int) -- (term , c)
                        else Nothing

            splitSign :: (String,Int) -> Maybe (String,String,Int,Bool)         -- (term,c) -> (term1,term2,c,isMinus)
            splitSign (str,c)
                | '+' `notElem` str && '-' `notElem` str = Nothing
                | otherwise = 
                     Just (takeWhile (\ch -> ch /= '+' && ch /= '-') str,
                      tail $ dropWhile (\ch -> ch /= '+' && ch /= '-') str,c,'-' `elem` str && '+' `notElem` str) 
            
            validateTermsHasPointXAndPointYAndPassCoef :: (String,String,Int,Bool) -> Maybe (String,String,Int,Bool)
            validateTermsHasPointXAndPointYAndPassCoef (lhs,rhs,c,isMinus) =
                let
                    hasPointBeforeXAndX = '.' `elem` lhs && 'x' `elem` tail (dropWhile (/= '.') lhs) 
                    hasPointBeforeYAndY = '.' `elem` rhs && 'y' `elem` tail (dropWhile (/= '.') rhs)
                in 
                    if hasPointBeforeXAndX && hasPointBeforeYAndY
                        then Just (takeWhile (/= '.') lhs,takeWhile (/= '.') rhs,c,isMinus)
                        else Nothing

            validateTermsAreNumbers :: (String,String,Int,Bool) -> Maybe LDE2             --validate coefs are numbers in term1 and two
            validateTermsAreNumbers (a,b,c,isMinus) = 
                let 
                    aInWords = words a
                    bInWords = words b

                    is1WordA  = length (words a) == 1
                    is1WordB  = length (words b) == 1

                    takeA = head aInWords
                    takeB = head bInWords

                    isNumA = is1WordA && all (`elem` digits) takeA
                    isNumB = is1WordB && all (`elem` digits) takeB

                in 
                    if isNumA && isNumB 
                        then if isMinus
                            then Just (LDE2 (read takeA:: Int) (-(read takeB:: Int)) c) -- b is with minus
                            else Just (LDE2 (read takeA :: Int) (read takeB :: Int) c)
                        else Nothing
                        



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

    toLDE2 "         -0      .       x      -          -3       .      y         =               7"
        Just (LDE2 0 3 7)

    toLDE2 "3 4.x-3.y= -7"  
        Nothing     
        
-}



--2) LDEN - Linear Diophantine Equation with N unknowns

data LDEn = LDEn{
    n :: Int,
    coefs :: [Int],
    rhs :: Int
}

instance Show LDEn where
    show :: LDEn -> String
    show (LDEn n l rhs) = "(LDEn " ++ show n ++ " " ++ show l ++ " " ++ show rhs ++ ")"


generateLDE2FromLDEn :: [Int] -> LDEn -> [LDE2]
generateLDE2FromLDEn ys (LDEn n coefs rhs) = 
       [LDE2 {a = coefs !! xi ,b = coefs !! xj, c = remainder} | 
        (xi,xj) <- fixCoefs n,
        chosen <- combination (n - 2) ys,
        let remainder = evalRemainder xi xj chosen
       ]
    where
        evalRemainder :: Int -> Int -> [Int] -> Int
        evalRemainder xi xj chosen = 
            let remainingCoefs = removeAtIndexes [xi, xj] coefs
            in sum (rhs : zipWith (*) chosen (map (*(-1)) remainingCoefs))
            

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
    
  
{-test:
    generateLDE2FromLDEn [1,2] (LDEn { n = 3, coefs = [1,4,1], rhs = 3 })
        [(LDE2 1 4 2),(LDE2 1 4 1),(LDE2 1 1 -1),(LDE2 1 1 -5),(LDE2 4 1 2),(LDE2 4 1 1)]

-}