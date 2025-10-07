--22.4. Да се напише функция, която намира броя на цифрите в десетичния
--запис на дадено естествено число.


sumDigits :: Int ->Int
sumDigits x = if x == 0 then 0 else x `mod` 10 + sumDigits(x `div` 10) 