{-
27.1. Да се дефинират функции над конфигурацията на везна, чрез които
може да се доавя тежест в лявата или дясната теглилка.
-}

data Scale = Scale{lhs :: Int ,rhs :: Int}
    deriving (Show)

addToLhs :: Scale -> Int -> Scale
addToLhs (Scale lhs rhs) w = Scale (lhs + w) rhs

addToRhs :: Scale -> Int -> Scale
addToRhs (Scale lhs rhs) w = Scale lhs (rhs + w)

