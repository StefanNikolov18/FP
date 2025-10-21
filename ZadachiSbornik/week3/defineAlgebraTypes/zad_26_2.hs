{-В един университет има студенти и преподаватели. Всеки студент има
факултетен номер, име и списък от имената на курсове, които посеща-
ва. Всеки преподавател има име, списък от имената на курсове, които
преподава и номер на кабинет. Да се дефинира алгебричен тип Member
със съответни конструктори за студент и преподавател.

а) По списък [Member] да се намери броя на студентите.
б) Да се дефинира функция attendees :: String -> [Member] -> Int,
която по име на предмет и списък от членове на университета на-
мира броя на студентите, които слушат предмета.
в) Да се дефинира функция classmembers :: String -> [Member] ->
[Member], която по име на предмет и списък от членове на универ-
ситета намира списък само на тези членове, които преподават или
слушат дадения предмет.
г) Да се дефинира функция namestitles :: [Member] -> [String],
която връща имената на всички членове на университета, като пред
името на всеки преподавател автоматично добавя титлата “проф.”.
д) (*) Да се дефинира функция bussiest :: [Member] -> Int, която
намира кабинета с най-много преподаватели.-}

data Member
    = Student
        { fn :: String
        , name :: String
        , courses :: [String]
        }
    | Professor
        { name :: String
        , courses :: [String]
        , cabinetNumber :: Int
        }
    deriving (Show)

members :: [Member]
members =
  [ Student "12345" "Ivan" ["Math", "CS"],
    Student "1445" "Stefan" ["Math","CS","DSA"],
    Professor "Dr. Petrov" ["CS", "AI"] 210
  ]
--a)
getCntMembers :: [Member] -> Int
getCntMembers [] = 0
getCntMembers (x : xs) = 1 + getCntMembers xs

--b)
attendees :: String -> [Member] -> Int
attendees course [] = 0 
attendees course (x : xs) =
    case x of 
        Student _ _ courses ->
            (if course `elem` courses then 1 else 0) + attendees course xs
        Professor _ _ _ -> 
            attendees course xs

--v)
classmembers :: String -> [Member] -> [Member]
classmembers _ [] = []
classmembers course (x : xs) = 
    case x of
        Student _ _ courses -> 
            if course `elem` courses then x : classmembers course xs else classmembers course xs
        Professor _ courses _ ->
             if course `elem` courses then x : classmembers course xs else classmembers course xs


--g)
namestitles :: [Member] -> [String]
namestitles [] = []
namestitles (x:xs) = 
    case x of
        Student _ name _ -> name : namestitles xs
        Professor name _ _ -> ("prof. " ++ name) : namestitles xs

--d)
bussiest :: [Member] -> Int
bussiest [] = -1
bussiest l
     | checkGreater50Percent result 0 0 l == True = result
     | otherwise = -1
    where
        result = boyarMooreAlgo 0 0 l
            
        boyarMooreAlgo :: Int -> Int -> [Member] -> Int
        boyarMooreAlgo m _ [] = m
        boyarMooreAlgo m c (x : xs) = 
            case x of
                Professor _ _ cabinet ->
                    if c == 0 then boyarMooreAlgo cabinet 1 xs else 
                        if m == cabinet then boyarMooreAlgo m (succ c) xs else boyarMooreAlgo m (pred c) xs
                Student {}  -> boyarMooreAlgo m c xs          

        checkGreater50Percent :: Int -> Int -> Int -> [Member] -> Bool
        checkGreater50Percent _ cnt allProf [] = cnt > (allProf `div` 2)
        checkGreater50Percent m cnt allProf (x : xs) =
            case x of 
                Professor _ _ cabinet -> 
                    if m == cabinet
                        then checkGreater50Percent m (succ cnt) (succ allProf) xs
                        else checkGreater50Percent m cnt (succ allProf) xs
                Student {} -> checkGreater50Percent m cnt allProf xs



