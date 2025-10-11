
f :: Int -> [Int]
f x = x : f x

has5 :: Int ->Bool
has5 x = (x > 0) && (mod x 10 == 5 || has5(div x 10))

x :: Integer
x = max 5 $ min 1 2
fact :: Int ->Int
fact 0 = 1
fact 1 = 1
fact x = x * (fact $ x - 1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = ((fib $ x - 1)  + (fib $ x - 2))

fst3 (x,_,_) = x
snd3 (_,y,_) = y
thr3 (_,_,z) = z

mylen [] = 0
mylen (_ : xs) = 1 + (mylen xs)

mysum [] = 0
mysum (x : xs) = x + (mysum xs)

progression [] = True
progression [_] = True
progression (x:y:xs) = x <= y && progression (y:xs) 

quadrant (x,y)
    | x > 0 && y > 0 = 1
    | x < 0 && y > 0 = 2
    | x < 0 && y < 0 = 3
    | x > 0 && y < 0 = 4
    | otherwise = 0

--if in list have equal elements
dup [] = False
dup (x:xs) = elem x xs || dup xs

--[a,b,c] . [d,e,f] = a*b + b*e + c*f
prod [] [] = 0
prod (x:xs) (y:ys) = x*y + prod xs ys
pref :: Int -> [Int] -> [Int]
pref _ [] = []
pref 0 _ = []
pref n (x : xs) = x : (pref (n-1) xs)
suf _ [] = []
suf n l 
    | n == length l = l
    | otherwise = suf n (tail l)