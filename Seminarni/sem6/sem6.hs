
import Prelude hiding (Functor,fmap,(<$>),Applicative,liftA2,(<*>))
--test
isPrime :: Int -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime n = all (\y -> n `rem` y /= 0) [2..n-1]

primeDouble :: [(Int,Int)]
primeDouble = [(x,y) | y <- [2..],x<- [2..y],isPrime x && isPrime y, abs(x - y) == 2]


--sem 6

--JSON Seialization
data Json = 
    JsonNull |
    JsonBool Bool |
    JsonDouble Double |
    JsonString String |
    JsonArray [Json] |
    JsonObject [(String,Json)]

joinMap :: (a -> String) -> String -> [a] -> String
joinMap f delim [] = ""
joinMap f delim  (x : xs) = foldl (\result current -> result ++ delim ++ f current ) (f x) xs

instance Show Json where
    show :: Json -> String
    show JsonNull = "null"
    show (JsonBool b) =
        let (x:xs) = show b
        in toEnum (fromEnum x - fromEnum 'A' + fromEnum 'a') : xs
    show (JsonDouble d) = show d
    show (JsonString s) = show s
    show (JsonArray l) = "[" ++ joinMap show ", " l ++ "]"
    show (JsonObject l) = "{" ++ joinMap (\(key,value) -> "\"" ++ key ++ "\":" ++ show value)  ",\n" l ++ "}"

instance JsonSerializable a => JsonSerializable (Maybe a) where
    toJson :: JsonSerializable a => Maybe a -> Json
    toJson Nothing = JsonNull
    toJson (Just value) = toJson value
     
data Student = Student {
    fn :: String,
    name :: String, 
    email :: String,
    phone :: String
}
instance JsonSerializable Student where
    toJson :: Student -> Json
    toJson (Student fn name email phone) = 
        JsonObject [
        ("fn", JsonString fn),
        ("name", JsonString name),
        ("email", JsonString email),
        ("phone", JsonString phone)
        ]


class JsonSerializable a where --interface
    toJson :: a -> Json
    toJsonString :: a -> String 
    toJsonString = show . toJson 



instance Semigroup Json where
    (<>) :: Json -> Json -> Json
    (JsonArray l1) <> (JsonArray l2) = JsonArray (l1 ++ l2)
    x <> (JsonArray l) = JsonArray (x : l)
    x <> y = JsonArray [x,y]

instance Monoid Json where
    mempty :: Json
    mempty = JsonArray []
    

serializeStudents :: [Student] -> Json
serializeStudents = mconcat . map toJson

 {-Functor class 
functor F :  a -> F a
(a -> b) -> (Fa -> F b)
id 
-}

class Functor f where 
    fmap :: (a -> b) -> f a -> f b

    (<$>) :: (a -> b) -> f a -> f b
    (<$>) = fmap

instance Functor Maybe where 
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)


instance Functor (Either a) where
    fmap :: (b -> c) -> Either a b -> Either a c
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)


--Monadi 
{-
neshto koeto dobavq dopulnitelna informaiciq e monadata

pure 
liftA2 

-}


class Functor f => Aplicative f where 
    (<*>) :: f (a-> b) -> f a -> f b
    pure :: a -> f a

    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 g fa fb = g <$> fa <*> fb 

    (*>) :: f a -> f b -> f b
    fa *> fb = liftA2 const fb fa 



    
instance Aplicative Maybe where

    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    _ <*> Nothing = Nothing
    Nothing <*> _ = Nothing
    (Just f) <*> (Just x) = Just (f x)


    pure :: a -> Maybe a
    pure = Just 


