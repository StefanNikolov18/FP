{-28.7. Да се дефинира функция split :: (a->Bool) -> [a] -> [[a]], получ-
ваваща предикат p и списък l. Елементите на l, удовлетворяващи p, се
считат за “разделители” в l и списъкът се разделя на части, обособени
от тези разделители. Например:
s p l i t (== ’ , ’) " part1 , p a r t ␣ 2 , p a r t 3 "
−> [ " p a r t 1 " , " p a r t ␣2" , " p a r t 3 " ]-}

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split pred list = 
    let x = takeWhile (not . pred) list
        y = dropWhile (not . pred) list
    in
        if not (null y) then x : split pred (tail y)
        else [x]

example :: String
example = " part1 , part 2 , part 3 "