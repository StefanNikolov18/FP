
{-29.6. Да се дефинира функция integrate :: (Double->Double) -> Int ->
Double -> Double -> Double, която по функция f : Double → Double
и цяло положително число N връща i(a, b) от тип i : Double × Double →
Double, изчисляваща приближено стойността на определния интегралR b
a f (x)dx (приемайки, че такъв съществува) по метода на трапеците,
по-специално Chained trapezoidal rule[8] с N на брой подинтервала. (вж.
фиг. 11.2)-}

integrate :: (Double -> Double) -> Int -> Double -> Double -> Double
integrate f n a b=
    let 
        h = (b - a) / fromIntegral n
        xs = [a + h * fromIntegral k | k <- [1 .. n-1]]
        s = foldr ((+) . f) 0 xs
    in 
        (h/2) * (f a + 2 * s + f b)