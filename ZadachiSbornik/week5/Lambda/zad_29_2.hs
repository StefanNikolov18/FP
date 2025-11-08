{-29.2. “Правоъгълна функция” наричаме функция f : Doube → Double, ко-
ято редува две стойности low и high, low < high, като ги разменя
със стъпка step по аргумента. Вж. фиг 4.2. Да се дефинира функция 
rectfn::Double -> Double -> Double -> (Double->Double), която по ар-
гументи low, high и step създава съответната правоъгълна функция.
Функцията на фигурата може да се създаде чрез rectfn 2 4 2.-}

rectfn :: Double -> Double -> Double -> (Double -> Double)
rectfn low high step = \x -> if even (floor (x /step)) then low else high
