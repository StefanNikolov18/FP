--25.6. Да се дефинира функция, която проверява дали даден низ е палиндром,
--т.е. дали се е еднакъв при четене от ляво на дясно и от дясно на ляво.

isPalindrome :: String -> Bool
isPalindrome str = str == reverse' str
        
        
reverse' :: String -> String
reverse' [] = []
reverse' str = reverseHelper str []
    where
        reverseHelper :: String -> String -> String
        reverseHelper [] res = res
        reverseHelper (x : xs) res = reverseHelper xs (x : res)
