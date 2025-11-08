module GameHaskel where

type Pos = (Int, Int)

data Tile = Road | Wall | Gold
    deriving (Show, Eq,Enum)

data Game = Game
    { pos :: Pos
    , world :: [[Tile]]
    } deriving (Show)

myWorld :: Game
myWorld = Game
    { pos = (0, 0)
    , world =
        [ [Road, Wall, Gold]
        , [Road, Wall, Road]
        , [Road, Road, Road]
        ]
    }

getWorld :: Game -> [[Tile]]
getWorld = world

left :: Game -> Game
left (Game (x, y) w) = Game (x - 1, y) w

right :: Game -> Game
right (Game (x, y) w) = Game (x + 1, y) w

up :: Game -> Game
up (Game (x, y) w) = Game (x, y - 1) w

down :: Game -> Game
down (Game (x, y) w) = Game (x, y + 1) w

foundGold :: Game -> Bool
foundGold (Game (x, y) w) = (w !! y) !! x == Gold


stuck :: Game -> Bool
stuck (Game (x, y) w) = dead w (x, y)


dead :: [[Tile]] -> Pos -> Bool
dead w (x, y) =
    x < 0 || y < 0 ||
    y >= length w ||
    x >= length (head w) ||
    (w !! y) !! x == Wall


move :: Game -> (Pos -> Pos) -> Maybe Game
move (Game p w) mv =
    if dead w (mv p)
        then Nothing
        else Just $ Game (mv p) w