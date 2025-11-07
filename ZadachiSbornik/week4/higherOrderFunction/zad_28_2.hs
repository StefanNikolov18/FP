{-28.2. Нека е даден списък l::[(Int,Int,Int)] с тройки (ai, bi, ci). С помощта
на map, fold и filter да се намерят:
а) Списъка от сумите на елементите на тройките [(ai + bi + ci)]
б) Тройка от сумите на отделните компоненти на елемнтите на l,
(P ai, P bi, P ci)
в) Броя на тройките, за които ai + bi > ci
г) Дали има поне една тройка, за която ai = bi = ci (True или False)
-}

type Triple = (Int, Int, Int)


--a)
sumTripleToInt :: Triple -> Int
sumTripleToInt (a,b,c) = a + b + c

sumTripleToList :: [Triple] -> [Int]
sumTripleToList  = map sumTripleToInt


--b)
sumTriples :: Triple -> Triple -> Triple
sumTriples (a1,b1,c1) (a2,b2,c2) = (a1 + a2, b1 + b2, c1 + c2)

sumTripletsInList :: [Triple] -> Triple
sumTripletsInList = foldr sumTriples (0,0,0)

--c)
isSumGreaterThanThird :: Triple -> Bool
isSumGreaterThanThird (a,b,c) = a + b > c 

getCntGreaterThanThird :: [Triple] -> Int
getCntGreaterThanThird list = length $ filter isSumGreaterThanThird list


--d)
allEqual :: Triple -> Bool
allEqual (a,b,c) = a == b && b == c

hasAllEqual :: [Triple] -> Bool
hasAllEqual l = not $ null (filter allEqual l)