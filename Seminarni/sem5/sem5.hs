import Prelude hiding (Maybe,Just,Nothing,maybe,mapMaybe,lookup,Either,Left,Right,either)

--1)
{-Задача 01 - Типа Maybe
Напишете своя версия на типа Maybe и за нея напишете следните функции:

isJust/isNothing - проверява дали типа е Just/Nothing
fromJust - приема стойност от тип Maybe и ако тя е Just връща стойността в нея
fromMaybe - приема стойност по подразбиране и стойност от тип Maybe. Ако вторият аргумент е Just, 
то функцията връща стойността му, в противен случай връща стойността по подразбиране
maybe - приема стойност по подразбиране, едноместна функция f и стойност от тип Maybe. 
Ако третият аргумент е Just, то функцията да прилага f върху стойността, в противен случай да върне стойността
 по подразбиране
catMaybes - приема списък от стойности от тип Maybe и връща списък от всички Just стойности
mapMaybe - приема функция f на един аргумент, която връща стойност от тип Maybe и списък. 
Функцията да прилага f над всички елементи и като резултат да върне само получените Just стойности
lookup - търси стойност в асоциативен списък по подаден ключ. Ако ключът не бъде намерен, връща Nothing
-}
data Maybe a = Just a | Nothing 
    deriving Show

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing x = not $ isJust x 

fromJust :: Maybe a -> a
fromJust (Just x) = x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just y) = y

maybe :: a -> (a -> a) -> Maybe a -> a
maybe x _ Nothing = x
maybe _ f (Just y) = f y


catMaybes :: [Maybe a] -> [a]
catMaybes = map fromJust . filter isJust 

mapMaybes :: (a -> Maybe b) -> [a] -> [b]
mapMaybes f = catMaybes . map f


lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup key [] = Nothing
lookup key ((l,r): xs)
    | key == l = Just r
    | otherwise = lookup key xs


{-
Задача 02 - Типа Either
Either is what's right or whatever's left

Напишете своя версия на типа Either и за нея напишете следните функции:

isLeft/isRight - проверява дали типа е Left/Right
fromLeft/fromRight - приема стойност по подразбиране и стойност от тип Either. Ако вторият аргумент е Left/Right, то функцията връща стойността му, в противен случай връща стойността по подразбиране
either - приема две едноместни функции f и g и стойност от тип Either. Ако третият аргумент е Left, то функцията да прилага f върху стойността иначе да прилага g
lefts/rights - приема списък от стойностти от тип Either и връща списък от всички Left/Right стойности
partitionEithers - приема списък от стойностти от тип Either и връща наредена двойка от списъци, съдържащи съответно всички Left и всички Right стойности
За референция, вижте функциите от модула Data.Either
-}

data Either a b = Left a | Right b
    deriving Show

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight x = not $ isLeft x

fromLeft :: a -> Either a b -> a
fromLeft _ (Left x) = x
fromLeft x (Right _) = x 

fromRight :: b -> Either a b -> b
fromRight x (Left _) = x
fromRight _ (Right x) = x

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right x) = g x

eitherToMaybeR :: Either a b -> Maybe b
eitherToMaybeR (Left _) = Nothing
eitherToMaybeR (Right x) = Just x

eitherToMaybeL :: Either a b -> Maybe a
eitherToMaybeL (Left x) = Just x
eitherToMaybeL (Right _) = Nothing

lefts :: [Either a b] -> [a]
lefts = mapMaybes eitherToMaybeL 

rights :: [Either a b] -> [b]
rights = mapMaybes eitherToMaybeR

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers l = 
    let
        left = [x | Left x <- l]
        right = [y | Right y <- l]
    in (left,right) 

{-
Задача 03 - Непразен списък
Напишете своя версия на типа NonEmpty, представляващ непразен списък. Използвайте (:|) за конструктор на типа. За него напишете следните функции:

fromList - конвертира списък към непразен списък, ако е възможно. Използвайте типа Maybe
asList - конвертира непразен списък към обикновен списък
mapNE, foldrNE, foldr1NE, maximumNE - аналог на съответните функции за списъци, но за непразен списък
mean и median - по подаден непразен списък връщат съответно средното и медианата на списъка
repeatNE - приема елемент x и генерира безкраен непразен списък съставен от x
iterateNE - приема функция на 1 аргумент f и елемент x. Функцията да генерира безкраен непразен списък от приложенията на f върху x
cycleNE - приема непразен списък и генерира безкраен непразен списък съставен от повтаряне на елементите на подадения списък
-}

data NonEmpty a = a :| [a]
    deriving Show

fromList :: [a] -> Maybe (NonEmpty a)
fromList [] = Nothing
fromList (x : xs) = Just $ x :| xs 

asList :: NonEmpty a -> [a]
asList (x :| xs) = x : xs

mapNE :: (a -> b) -> NonEmpty a -> NonEmpty b
mapNE f (x :| xs) = f x :| map f xs


foldrNE :: (a -> b -> b) -> b ->  NonEmpty a -> b
foldrNE op z (x :| xs) = op x (foldr op z xs)

foldr1NE :: (a -> a -> a) -> NonEmpty a -> a
foldr1NE op (x :| xs) = foldr1 op (x:xs)

mean :: Fractional a => NonEmpty a -> a
mean l@(x :| xs) =
    let total = foldr (+) x xs
        cnt = fromIntegral (1 + length xs)
    in total/cnt

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
    let pivot = x
        left = [x | x <- xs , x < pivot]
        right = [y | y <- xs , y >= pivot]
    in quickSort left ++ [pivot] ++ quickSort right


median :: (Fractional a ,Ord a) => NonEmpty a -> a
median (x :| xs) =
    let arr = quickSort (x:xs)
        size = (1 + length xs)
    in if even size
        then 
            let i = size `div` 2
            in (arr !! (i - 1) + arr !! i) / 2
        else 
            arr !! (size `div` 2)


repeatNE :: a -> NonEmpty a
repeatNE x = x :| repeat x

iterateNE :: (a -> a) -> a -> NonEmpty a
iterateNE f x = x :| iterate f (f x)

cycleNE :: NonEmpty a -> [a]
cycleNE (x :| xs) = cycle (x : xs)

{-
Задача 04 - Естествено число
Напишете потребителски тип Nat, представляващ естествено число. Чрез него напишете следните функции:

plus - събира 2 естествени числа
multiply - умножава 2 естествени числа
fromInt - конвертира цяло число към естествено, ако е възможно. Използвайте типа Maybe
toInt - конвертира естествено число към цяло
-}

data Nat = Zero | Succ Nat deriving Show

plus :: Nat -> Nat -> Nat
plus Zero y = y
plus (Succ x) y = plus x (Succ y)

multiply :: Nat -> Nat -> Nat
multiply Zero y = Zero
multiply (Succ Zero) y = y
multiply (Succ x) y = plus y (multiply x y)
 
fromInt :: Int -> Maybe Nat
fromInt n
    | n < 0 = Nothing
    | n == 0 = Just Zero
    | n > 0 = Just $ Succ $ fromJust $ fromInt (n - 1)

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ x) = 1 + toInt x

{-
Задача 05 - Верига
Напишете потребителски тип верига (Chain), която наподобява списък, но позволява константно време на 
операцията конкатенация на две вериги. За целта напишете 3 конструктора - Empty представляващ празна верига,
 Singleton представляващ верига, съдържаща само 1 елемент и Append представляващ верига, изградена от 2 други вериги.
  Напишете следните функции:

(<:) и (>:) - приемат елемент и верига и добавят елемента съответно в началото и края на веригата
headChain - връща първия елемент на верига, ако тя не е празна
tailChain - приема верига и връща нова верига, която съдържа елементите от подадената в същия ред, но без първия. 
Не е задължително новата верига да има същата структура като подадената
append - конкатенира две вериги
mapChain - аналог на map за списъци, но за верига
foldlChain - аналог на foldl за списъци, но за верига
toList - приема верига и връща списък от елементите на веригата в същия ред
listify - приема верига и връща нова верига, съдържаща същите елементи като подадената, но с променена структура - 
тя трябва да наподобява тази на списъка и изискването е всеки ляв елемент на Append верига да бъде Singleton 
(ако веригата не е празна разбира се)
-}


data Chain a = Empty | Singleton a | Append (Chain a) (Chain a)
    deriving Show

(<:) :: a -> Chain a -> Chain a
x <: Empty = Singleton x
x <: Singleton y = Append (Singleton x) (Singleton y)
x <: Append c1 c2 = Append (x <: c1) c2

(>:) :: a -> Chain a -> Chain a
x >: Empty = Singleton x
x >: Singleton y = Append (Singleton y) (Singleton x)
x >: Append c1 c2 = Append c1 (x >: c2)

headChain :: Chain a -> Maybe a
headChain Empty = Nothing
headChain (Singleton x) = Just x
headChain (Append c1 c2) = headChain c1

tailChain :: Chain a -> Chain a
tailChain (Append (Singleton x) c2)= c2
tailChain (Append c1 c2) = Append (tailChain c1) c2 


append :: Chain a -> Chain a -> Chain a
append c Empty = c
append c (Singleton x) = Append c (Singleton x)
append c (Append c1 c2) = Append (append c c1) c2

mapChain :: (a -> a) -> Chain a -> Chain a
mapChain _ Empty = Empty
mapChain f (Singleton x) = Singleton (f x)
mapChain f (Append x y) = Append (mapChain f x) (mapChain f y)

foldrChain :: (b -> a -> b) ->b -> Chain a -> b 
foldrChain _ nv Empty  = nv
foldrChain op nv (Singleton x) = nv `op` x 
foldrChain op nv (Append c1 c2) =
    let acc = foldrChain op nv c1
    in foldrChain op acc c2

foldlChain :: (b -> a -> b) -> b -> Chain a -> b 
foldlChain op nv chain  = foldlChainIter op nv [chain]
    where
        foldlChainIter :: (b -> a -> b) -> b -> [Chain a] -> b
        foldlChainIter op nv [] = nv
        foldlChainIter op nv (Empty: cs) = foldlChainIter op nv cs
        foldlChainIter op nv ((Singleton x) : cs) = foldlChainIter op (op nv x) cs
        foldlChainIter op nv ((Append c1 c2) : cs) = foldlChainIter op nv (c1:c2:cs)



toList :: Chain a -> [a]
toList Empty = []
toList (Singleton x) = [x]
toList (Append c1 c2) = toList c1 ++ toList c2  

listify :: Chain a -> Chain a
listify Empty = Empty
listify (Singleton x) = Singleton x
listify (Append c1 c2) = appendRight (listify c1) (listify c2)
    where
        appendRight :: Chain a -> Chain a -> Chain a
        appendRight Empty ys = ys
        appendRight (Singleton x) ys = Append (Singleton x) ys
        appendRight (Append x y) ys = Append x (appendRight y ys)

{-
Задача 06 - Валидация и създаване на студенти
Напишете запис (record) Student, който представлява студент в университет.
 Един студент съдържа факултетен номер, име, имейл и телефонен номер, който обаче не е задължителен.

Напишете функция createStudent, която приема необходимите данни на един студент, 
валидира ги и в случай на успех връща новосъздадения студент. Валидациите са следните:

факултетният номер трябва да има дължина от точно 10 символа
името трябва да се състои от 3 думи - първо, бащино и фамилно име
имейлът трябва да съдържа един символ @ и поне един символ . след него
телефонният номер трябва да започва с 0 и да има дължина от точно 10 символа
Ако някои от тези валидации не са успешни, функцията да връща непразен списък,
 съдържащ подходящи съобщения спрямо вида на грешките (свободни сте да изберете формата на съобщенията).
-}

data Student = Student{
    fn :: String,
    name :: String,
    email :: String,
    phone :: Maybe String

} deriving Show

data ValidationError
  = InvalidFnError
  | InvalidNameError
  | InvalidEmailError
  | InvalidPhoneError
  deriving (Show)

isValidFn :: String -> Bool
isValidFn (x : xs) = (1 + length xs) == 10

--words String - splitva go na tokeni

isValidName :: String -> Bool
isValidName name = cntWhiteSpaces name == 2
    where
        cntWhiteSpaces :: String -> Int
        cntWhiteSpaces [] = 0
        cntWhiteSpaces (x : xs)
            | x == ' ' = 1 + cntWhiteSpaces xs
            | otherwise = cntWhiteSpaces xs


isValidNameBetter :: String -> Bool
isValidNameBetter name = length (words name) == 2

isValidEmail :: String -> Bool
isValidEmail  = isValidEmailIter False False
    where
        isValidEmailIter :: Bool -> Bool -> String -> Bool
        isValidEmailIter seenAt seenPoint [] = seenAt && seenPoint
        isValidEmailIter seenAt seenPoint (x : xs)
            | x == '@' && not seenAt = isValidEmailIter True seenPoint xs
            | x == '@' && seenAt = False
            | x == '.' && seenAt = isValidEmailIter seenAt True xs
            | x == '.' && seenPoint = False
            | otherwise = isValidEmailIter seenAt seenPoint xs

--dropWhile
isValidEmailBetter :: String -> Bool
isValidEmailBetter  email =
    let rest = dropWhile (/= '@') email
    in not (null rest) && '.' `elem` tail rest   

isValidPhone :: Maybe String -> Bool
isValidPhone Nothing = True
isValidPhone (Just (x : xs)) =
    let l = (1 + length xs)
    in x == '0' && l == 10

createStudent :: String -> String -> String -> Maybe String -> Either [ValidationError] Student
createStudent fn name email phone = 
    let
        errors =
            [InvalidFnError | not (isValidFn fn)]++
             [ InvalidNameError | not (isValidName name) ] ++
            [ InvalidEmailError| not (isValidEmail email) ] ++
            [ InvalidPhoneError| not (isValidPhone phone) ]
    in
        if null errors
            then Right (Student fn name email phone)
            else Left errors


