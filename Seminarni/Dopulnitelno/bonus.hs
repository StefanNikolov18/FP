{-# LANGUAGE EmptyDataDeriving #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- Задача 1: Да се дефинират типове Suit и Rank
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show, Ord)
type Rank = Int

-- Задача 2: Да се дефинира тип Card, който има следните полета: rank :: Rank, suit :: Suit
data Card = Card {rank :: Rank , suit :: Suit} deriving Show

getRank :: Card -> Rank
getRank = rank

getSuit :: Card -> Suit
getSuit = suit

-- Задача 3: Да се напише функция, която генерира стандартно тесте от 52 карти
generateCardsBySuit :: [Int] -> Suit -> [Card]
generateCardsBySuit [] _ = []
generateCardsBySuit (r : rx) suit = Card r suit : generateCardsBySuit rx suit


generateCards :: [Card]
generateCards =
    let rankSet = [2..14]
    in 
        generateCardsBySuit rankSet Hearts ++
        generateCardsBySuit rankSet Diamonds ++
        generateCardsBySuit rankSet Clubs ++
        generateCardsBySuit rankSet Spades

-- Задача 3: Да се дефинира тип Player, който има следните полета: name :: String, age :: Int, Hand :: [Card]
data Player = Player{
    name :: String,
    age :: Int,
    hand :: [Card]
} deriving Show

getName :: Player -> String
getName = name

getAge :: Player -> Int
getAge = age

getHand :: Player -> [Card]
getHand = hand

-- Задача 4: Да се напише функция, която приема списък от играчи и им раздава всички карти последователно
dealCards :: [Player] -> [Card] -> [Player]
dealCards [] _ = []
dealCards players [] = players
dealCards (p : ps) (c : cs) = 
    dealCards (ps ++ [Player {name = getName p , age = getAge p, hand = c : getHand p}]) cs

-- Задача 5: Да се напише функция, която приема списък от играчи и връща името на играча с най-висока сума
-- от ранговете на картите в ръката му
getSumRanks :: [Card] -> Int
getSumRanks = foldr ((+) . getRank) 0

mvp :: [Player] -> Maybe Player
mvp [] = Nothing
mvp (p : ps) = Just (mvpHelper p ps)
    where
        mvpHelper :: Player -> [Player] -> Player
        mvpHelper pl [] = pl
        mvpHelper pl (x : xs)
            | getSumRanks (getHand pl) < getSumRanks (getHand x) = mvpHelper x xs
            | otherwise = mvpHelper pl xs

-- Задача 6: Да се напише функция, която приема играч и връща всички червени карти от ръката му
isRedCard :: Card -> Bool
isRedCard c = getSuit c == Hearts || getSuit c == Diamonds

getRedCardsFromPlayer :: Player -> [Card]
getRedCardsFromPlayer p= getRedCards $ getHand p
    where
        getRedCards :: [Card] -> [Card]
        getRedCards = filter isRedCard

-- Задача 7: Да се напише функция, която генерира стандартно тесте за игра на белот
isBelotCard :: Card -> Bool
isBelotCard c = getRank c >= 7

generateBelotCards :: [Card]
generateBelotCards = filter isBelotCard generateCards

-- Задача 8: Да се напише функция, която цепи тестето на подадено от играча място
splitCardsOnPoint :: [Card] -> Int -> [Card]
splitCardsOnPoint [] _ = []
splitCardsOnPoint cards 0 = cards
splitCardsOnPoint (c : cs) p = splitCardsOnPoint (cs ++ [c]) (p - 1)


-- Задача 9: Да се напише функция, която цепи тестето n пъти. Изберете произволен pattern,
-- с който да направите безкрайния списък, от който се избира място за цепене
splitCardsNTimesIter :: Int -> [Int] -> [Card] -> Int -> [Card]
splitCardsNTimesIter _ _ cards 0 = cards
splitCardsNTimesIter i list cards n =
    splitCardsNTimesIter (i + 1) list (splitCardsOnPoint cards (list !! i)) (n - 1)

splitCardsNTimes :: [Card] -> Int -> [Card]
splitCardsNTimes [] _ = []
splitCardsNTimes c 0  = c
splitCardsNTimes cards n = 
    let list = [1,5 ..]
    in splitCardsNTimesIter 0 list cards n 

    
-- Задача 10: Да се напише функция, която раздава карти (приемете, че веднага е имало обявяване)
giveCards :: [Card] -> Int -> [Card]
giveCards [] _ = []
giveCards cards n = take n cards

dealNumberCards :: [Player] -> [Card] -> Int -> [Player]
dealNumberCards [] _ _ = []
dealNumberCards (p : pl) cards howMany = Player {name = getName p,age = getAge p , hand = getHand p ++ (giveCards cards howMany)}:
    dealNumberCards pl (drop howMany cards) howMany

dealBelotCards :: [Player] -> [Card] -> [Player]
dealBelotCards [] _ = []
dealBelotCards p [] = p
dealBelotCards players cards =
    let deal = [3,2,3]
    in dealBelotCardsHelper deal players cards
        where 
            dealBelotCardsHelper :: [Int] -> [Player] -> [Card] -> [Player]
            dealBelotCardsHelper [] pl _ = pl
            dealBelotCardsHelper (x : xs) pl c = 
                dealBelotCardsHelper xs (dealNumberCards pl c x) (drop (length pl * x) c)

--test
playersA :: [Player]
playersA =
  [ Player "Alice" 25 []
  , Player "Bob"   30 []
  , Player "Charlie" 22 []
  ]

-- Задача 11: Да се напише функция, която приема карта и играч и избира коя карта от ръката му да играе 
    -- По възможност трябва да отговаря на боя и да е с по-висок ранк
    -- Ако играчът няма възможност да даде карта с по-висок ранк от същата боя, трябва да даде картата с най-нисък ранк от тази боя
    -- Ако играчът няма възможност да даде карта от същата боя, трябва да даде картата с най-нисък ранк в ръката си
    -- Ако има повече от една карта с най-нисък ранк да приоризира боите както в стандартна игра на белот
isSameSuitCard :: Suit -> Card ->  Bool
isSameSuitCard suit card = getSuit card == suit

isHigherRankCard :: Rank -> Card -> Bool
isHigherRankCard rank card = getRank card > rank

higherRank :: Card -> Card -> Bool
higherRank c1 c2 = getRank c1 > getRank c2

lowerRank :: Card -> Card -> Bool
lowerRank c1 c2 = getRank c1 < getRank c2

getCardRankByOrder :: (Card -> Card -> Bool) -> [Card] -> Maybe Card
getCardRankByOrder _ [] = Nothing
getCardRankByOrder pred (c : cs) = getCardRankByOrderHelper c pred cs
    where 
        getCardRankByOrderHelper :: Card -> (Card -> Card -> Bool) -> [Card] -> Maybe Card
        getCardRankByOrderHelper highest _ [] = Just highest
        getCardRankByOrderHelper highest pred (x : xs)
            | x `pred` highest =  getCardRankByOrderHelper x pred xs
            | otherwise = getCardRankByOrderHelper highest pred xs

getCardFromPlayerHand :: Player -> Card -> Maybe Card
getCardFromPlayerHand player = getSpecialCardFromHand (getHand player)
    where
        getSpecialCardFromHand :: [Card] -> Card -> Maybe Card
        getSpecialCardFromHand [] _ = Nothing
        getSpecialCardFromHand hand card =
             let
                cardsWithSameSuit = filter (isSameSuitCard $ getSuit card) hand
                cardsWithHighestRank = filter (isHigherRankCard $ getRank card) hand
                cardsWithBoth = filter (isHigherRankCard $ getRank card) (filter (isSameSuitCard (getSuit card)) hand) --filter(filter)
            in 
                if not $ null cardsWithBoth  
                then getCardRankByOrder higherRank cardsWithBoth
                else if null cardsWithSameSuit 
                    then getCardRankByOrder lowerRank hand 
                    else getCardRankByOrder lowerRank cardsWithSameSuit


-- Задача 12: Да се напише функция, която преценя кой играч е взел ръката. Предполагаме, че първия играч в списъка е бил под ръка
getGreaterCardByRank :: [Card] -> Card
getGreaterCardByRank (x : xs) = getGreaterCardByRankHelper x xs
    where
        getGreaterCardByRankHelper :: Card -> [Card] -> Card
        getGreaterCardByRankHelper greater [] = greater
        getGreaterCardByRankHelper greater (c : cs)
            | getRank c > getRank greater = getGreaterCardByRankHelper c cs
            | otherwise = getGreaterCardByRankHelper greater cs

isSameSuit :: Card -> Card -> Bool
isSameSuit c1 c2 = getSuit c1 == getSuit c2

isGreaterCard :: Card -> Card -> Bool
isGreaterCard c1 c2 = getRank c1 > getRank c2

whoTakeTheHand :: [Player] -> Maybe Player
whoTakeTheHand [] = Nothing
whoTakeTheHand (p:ps) = whoTakeTheHandHelper (head (getHand p)) p ps
    where
        whoTakeTheHandHelper :: Card -> Player ->[Player] -> Maybe Player
        whoTakeTheHandHelper greaterCard beater [] = Just beater
        whoTakeTheHandHelper greaterCard beater (x : xs) = 
            let 
                filteredCardsXBySuit = filter (isSameSuit greaterCard) (getHand x)
            in 
                if null filteredCardsXBySuit
                    then whoTakeTheHandHelper greaterCard beater xs
                    else 
                        let xGreaterCard = getGreaterCardByRank filteredCardsXBySuit
                        in 
                            if isGreaterCard xGreaterCard greaterCard
                            then  whoTakeTheHandHelper xGreaterCard x xs
                            else  whoTakeTheHandHelper greaterCard beater xs

-- Задача 13: Да се напише функция, която према ръка и играчите и размества списъка така,
-- че взелият играч да е под ръка

underHand :: [Card] -> [Player] -> [Player]
underHand _ [] = []
underHand kompots persons = underHand (getRol) (whoTakeTheHand)

