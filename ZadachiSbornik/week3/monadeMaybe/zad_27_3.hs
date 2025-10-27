{-
27.3. Да се илюстрира няколкократно добавяне на тежести чрез приложение
на оператора >>=.
-}

--Just (Scale 20 25) >>= (`addToLhs` 10) >>= (`addToRhs` 5) >>= (`addToLhs` 30) >>= (`addToRhs` 20)
--Just (Scale 20 25) >>= (`addToLhs` 10) >>= (`addToRhs` 5) >>= (`addToLhs` 30) >>= (`addToRhs` 20) >>= (`addToLhs` 41)