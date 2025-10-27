{-
27.2. Функциите за добавяне на тежести да използват монада Maybe за трети-
ране на невалидни конфигурации. Считаме, че везната е в недвалидна
конфигурация, ако някоя от тежестите е над 100 или разликата на те-
жестите е над 50.
-}

data Scale = Scale {lhs::Int,rhs::Int}
    deriving(Show)

validScale :: Scale -> Bool
validScale (Scale l r) =
    l <= 100 && r <= 100 && abs(l - r) <= 50 


addToLhs :: Scale -> Int -> Maybe Scale
addToLhs (Scale l r) w = 
    let newScale = Scale (l + w) r
    in if validScale newScale 
        then Just newScale
        else Nothing


addToRhs :: Scale -> Int -> Maybe Scale
addToRhs (Scale l r) w =
    let newScale = Scale l (r + w)
    in if validScale newScale
        then Just newScale
        else Nothing


