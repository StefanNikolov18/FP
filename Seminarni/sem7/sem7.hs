import Prelude hiding (Monad, (>>=), return, (>>))

data Chain a = Empty | Singleton a | Append (Chain a) (Chain a)
 deriving (Show,Eq)

instance Functor Chain where
    fmap :: (a -> b) -> Chain a -> Chain b
    fmap _ Empty = Empty
    fmap f (Singleton x) = Singleton (f x)
    fmap f (Append c1 c2) = Append (fmap f c1) (fmap f c2)



instance Applicative Chain where

    pure :: a -> Chain a
    pure = Singleton

    (<*>) :: Chain (a -> b) -> Chain a -> Chain b
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    Singleton f <*> c = fmap f c
    Append f1 f2 <*> c = Append (f1 <*> c) (f2 <*> c)


--(>=>) :: m a -> (a -> m b) -> m b -fish operator
--(>>=) :: m a -> (a -> m b) -> m b -bind operator
--Monad 
class Applicative m => Monad m where 
    (>>=) :: m a -> (a -> m b) -> m b

    return :: a -> m a
    return = pure

    (>>)   :: m a -> m b -> m b
    (>>) = (*>)


instance Monad Maybe where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= f = Nothing
    (Just x) >>= f = f x


--do moga da izpolzvam samo za monadi


{-composeList :: [(Int, Int)]
composeList = do
    x <- [1..5]
    y <- [1..5]
    return (x,y) -}

instance Monad Chain where
    (>>=) :: Chain a -> (a -> Chain b) -> Chain b
    Empty >>= _ = Empty
    Singleton x >>= f = f x
    Append c1 c2 >>= f = Append (c1 >>= f) (c2 >>= f)

{-composeChain :: Chain Int
composeChain = do
    x <- Append(Singleton 1) (Singleton 2)
    y <- Append(Singleton 10) (Singleton 20)
    return (x + y) -}

--monad IO

getValue :: Read a => IO a 
getValue = read <$> getLine



