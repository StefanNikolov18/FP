{-
27.4. Да се дефинира функция state, която по конфигурация на везна връща
лява проекция, ако лявата тежест и по-голяма или равна на дясната.
Функцията да връща дясна проекция в противен случай.
Пример: Следният израз ще има оценка 16:
let myscale = (5,3) >>= left 10 >> right 10 >> right 3 in
(state myscale) myscale
-}

-- Тип за везна
type Scale = (Int, Int)

-- Добавяне в ляво (като Maybe)
left :: Int -> Scale -> Maybe Scale
left w (l,r) = Just (l + w, r)

-- Добавяне в дясно
right :: Int -> Scale -> Maybe Scale
right w (l,r) = Just (l, r + w)

-- state избира ляво или дясно
state :: Scale -> String
state (l,r)
    | l >= r    = "left"
    | otherwise = "right"

