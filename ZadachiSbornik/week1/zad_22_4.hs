--Да се напише функция, която намира броя на цифрите в десетичния
--запис на дадено естествено число.

cntDigits :: Int -> Int
cntDigits x = if abs x < 10 then 1 else 1 + cntDigits(x `div` 10)
