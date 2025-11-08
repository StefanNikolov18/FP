{-29.3. Да се дефинира функция createfn :: [(a,b)]->(a->b), която по даден
списък от двойки (ai, bi), връща функция f : a → b дефинирана за всич-
ки ai, така че f (ai) = bi. Например:
c r e a t e f n [ ( 1 , 2 ) , ( 2 , 4 ) , ( 3 , 6 ) ] 2 −> 4-}

createfn :: Eq a => [(a,b)] -> (a -> b)
createfn ((l,r) : xs) = \x -> if x == l 
    then r else createfn xs x

fn :: [(Int,Int)]
fn = [(1,2),(2,4),(3,6)]