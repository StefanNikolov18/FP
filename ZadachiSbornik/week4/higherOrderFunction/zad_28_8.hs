{-28.8. С помощта на map, mapMaybe, filter и fold да се намерят:
а) Броя на препятсвията в даден лабиринт
б) Броя съседи на целта, които не са препятсвия
в) Дали целта е оградена изцяло от препятствия (True или False)-}

 import GameHaskel

--a)
getTilesInWorld :: Game -> [Tile]
getTilesInWorld g = concat (getWorld g)

getCntWalls :: Game -> Int
getCntWalls g = length $ filter (== Wall) (getTilesInWorld g)

--b)
getRowCol :: Int -> Int -> Pos
getRowCol cols index = (index `div` cols, index `mod` cols)

getPosGold :: Game -> Pos
getPosGold g = 
    let 
        tiles = getTilesInWorld g
        goldArr = filter (\(t, _) -> t == Gold) (zip tiles [0..])
        index = snd (head goldArr)
        cols = round (sqrt (fromIntegral (length tiles)))
    in getRowCol cols index

cntPathAroundGold :: Game -> Int
cntPathAroundGold g = 


