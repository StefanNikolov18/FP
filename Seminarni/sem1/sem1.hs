
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

--5)Напишете функция distance, която приема 2 двойки от координати на точки и 
--намира разстоянието между точките в равнината.
type Point = (Double,Double)
distance :: Point -> Point -> Double
distance (x1,y1) (x2,y2) =  sqrt ((x2 - x1)*(x2-x1) + (y2 - y1)*(y2 - y1))


--6)Напишете функция, която приема наредена тройка от две цели числа и едно рационално число.'
-- Функцията трябва да раздели третото число на първото и
-- да добави към резултата второто. Aко деленето е невъзможно да се върне резултат 0.

type Triple = (Int,Int,Double)
evalTriplet :: Triple -> Double
evalTriplet (x,y,z)
    | x == 0 = 0
    | otherwise = (fromIntegral x / z) + fromIntegral y

 --7) Напишете оператор ~=, който проверява дали две рационални числа са приблизително равни.
(~=) :: Double -> Double -> Bool
x ~= y = abs (x - y) < 0.000001    

--8)Напишете функция factorial, която приема цяло число n и намира n!. 
--Реализирайте я по 3 начина - чрез pattern matching, guards и if then else.
--a)
factorialA :: Int -> Int
factorialA 0 = 1
factorialA n = n * factorialA(n-1)

--b)
factorialB :: Int->Int
factorialB n
    | n == 0 = 1
    | otherwise = n* factorialB(n-1)

 --c)
factorialC :: Int->Int
factorialC n = if n == 0 then 1 else n*factorialC(n - 1)

--9)Напишете функция sum', която приема 2 цели числа, и намира сумата на числата в затворения интервал,
-- определен от тях. Използвайте рекурсия, а не готовата формула.
sum' :: Int -> Int -> Int
sum' a b
    | b < a = 0
    | otherwise = a + sum'(a+1) b

--10)Напишете функция reverse', която приема цяло число, 
--и връща числото, съставено от същите цифри, но в обратен ред.
reverse' :: Int -> Int
reverse' = reverseHelper 0
    where 
        reverseHelper :: Int -> Int -> Int
        reverseHelper r 0 = r
        reverseHelper r n = reverseHelper (r*10 + (n `rem` 10))  (div n 10)

--11)Напишете функция gcd', която приема 2 цели числа, и намира техния най-голям общ делител.
gcd' :: Int -> Int -> Int
gcd' a b = gcdHelper (abs a) (abs b)
    where 
        gcdHelper :: Int -> Int -> Int
        gcdHelper a b 
            | a == b = a
            | a > b = gcdHelper (a-b) b
            |otherwise = gcdHelper a (b-a)        