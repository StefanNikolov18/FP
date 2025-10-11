
--1)Напишете функция sign, която приема число n и връща "positive", 
--ако е положително, "negative", ако е отрицателно, и "zero", ако е 0.
sign :: Int -> String
sign 0 = "zero"
sign n 
    | n < 0 = "negative"
    | n > 0 = "positive"
    
 --2)Напишете функция, която приема 3 аргумента - коефициентите на квадратно уравнение. 
 --Функцията да връща броя на корените на квадратното уравнение.   
rootsCnt :: Int -> Int -> Int -> Int
rootsCnt a b c
    | a == 0 && b == 0 = 0
    | a == 0 = 1
    | discriminant < 0 = 0
    | discriminant == 0 = 1
    | otherwise = 2
    where
        discriminant = (b * b) - (4 * a * c)


--3)Напишете функция superNumber, която приема 3 числа и 
--намира произведението на най-голямо и най-малкото, събрано със средното.
superNumber :: Int->Int -> Int -> Int
superNumber x y z = maximum [x,y,z] * minimum [x,y,z] + middle
    where 
        middle 
            | (x /= maximum [x,y,z]) && (x /= minimum [x,y,z]) = x
            | (y /= maximum [x,y,z]) && (y /= minimum [x,y,z]) = y
            | otherwise = z

--4) Напишете функция, която приема комплексно число като наредена двойка от
-- реална и имагинерна част. Функцията да намира модулът на това число. 
--Изпробвайте функцията curry върху вашата функция.  
modulus :: Int -> Int -> Double
modulus a b = sqrt(fromIntegral (a*a + b*b))

--5)Напишете функция, която приема наредена тройка от две цели числа и едно рационално число.'
-- Функцията трябва да раздели третото число на първото и
-- да добави към резултата второто. Aко деленето е невъзможно да се върне резултат 0.

type Triple = (Int,Int,Double)
evalTriplet :: Triple -> Double
evalTriplet (x,y,z)
    | x == 0 = 0
   -- | otherwise = fromIntegral (x/z) + fromIntegral y

 --6) Напишете оператор ~=, който проверява дали две рационални числа са приблизително равни.
(~=) :: Double -> Double -> Bool
x ~= y = abs (x - y) < 0.000001    