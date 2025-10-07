double :: Int -> Int
double x = x*2

triple :: Int -> Int
triple x = x*3

f :: Int -> Int
f x = x + 1

list :: [Integer]
list = [1,2,3]
emptyList :: [a]
emptyList = []

--infinity list
inf :: [Integer]
inf = [100..]
fInf :: Integer
fInf = head inf

--returns list of interval from x to y
interval :: (Ord t, Num t) => t -> t -> [t]
interval x y = if x > y then [] else x : (interval (x + 1) y)

--pair
s :: (String, Integer)
s = ("Stefan",21)

--returns count of the list
mcount :: Num a1 => [a2] -> a1
mcount l = if null l then 0 else 1 + (mcount(tail l))

--make pairs in list
zipList :: [(Integer, Integer)]
zipList = zip [1,2,3] [4,5,6]

-- tovq !! - e indeksaciq na spisyk
-- tova ++ e konkatenaciq na spisyci