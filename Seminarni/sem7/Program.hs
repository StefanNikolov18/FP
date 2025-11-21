module Main where

getValue :: Read a => IO a
getValue = read <$> getLine 

getValues :: Read a => IO [a]
--getValues = map read . words <$> getLine
getValues = do
    line <- getLine
    let values = words line 
    let result = map read values
    return result


printList :: Show a => String -> [a] -> IO ()
{-printList delim l = 
    let strings = map show l
        result = foldr (\current result -> if result == ""
        then current else current ++ delim ++ result) "" strings 
    in putStrLn result -}
printList delim = mapM_ (putStrLn . show)



 

main :: IO () 
main = do
    x <- getValue :: IO Int 
    print x