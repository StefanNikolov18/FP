
--25.1. Дефинирайте функция say, която по едноцифрено цяло число връща
--неговото наменование. Например, say 3 → ”three”.
say :: Int -> String
say n
    | n == 0 = "Zero"
    | n == 1 = "One"
    | n == 2 = "Two"
    | n == 3 = "Three"
    | n == 4 = "Four"
    | n == 5 = "Five"
    | n == 6 = "Six"
    | n == 7 = "Seven"
    | n == 8 = "Eight"
    | n == 9 = "Nine"
    | otherwise = "Must be 1 digit number"
