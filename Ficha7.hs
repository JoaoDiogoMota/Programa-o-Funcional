--1)
data ExpInt = Const Int
              | Simetrico ExpInt
              | Mais ExpInt ExpInt
              | Menos ExpInt ExpInt
              | Mult ExpInt ExpInt


e::ExpInt
e = (Const 2) `Mais`((Const 3) `Mult` (Const 5))
--a)
calcula :: ExpInt -> Int
calcula (Const a) = a
calcula (Simetrico a) = - abs(calcula a)
calcula (Mais a b) = (calcula a) + (calcula b)
calcula (Menos a b) = (calcula a) - (calcula b)
calcula (Mult a b) = (calcula a) * (calcula b)

--b)
infixa :: ExpInt -> String
infixa (Const a) = show a
infixa (Simetrico a) = "-(" ++ (infixa a) ++ ")"
infixa (Mais a b) = (infixa a) ++ "+" ++ (infixa b)
infixa (Menos a b) = (infixa a) ++ "-" ++ (infixa b)
infixa (Mult a b) = "(" ++ (infixa a) ++ "*" ++ (infixa b) ++ ")"

--c)
posfixa :: ExpInt -> String
posfixa (Const a) = show a
posfixa (Simetrico a) = "-(" ++ (posfixa a) ++ ")"
posfixa (Mais a b) = (posfixa a) ++ (posfixa b) ++ "+"
posfixa (Menos a b) = (posfixa a) ++ (posfixa b) ++ "-"
posfixa (Mult a b) = (posfixa a) ++ (posfixa b) ++ "*"

--2

data RTree a = R a [RTree a]


arvR = R 2 [R 1 [R 4 [],R 3 [R 1 []],R 5 []], R 3 [R 2 [], R 3 []] ]

--a)
soma :: Num a => RTree a -> a
soma (R a []) = a
soma (R a l) = a + sum(map soma l)

--b)
altura :: RTree a -> Int
altura (R a []) = 1
altura (R a l) = 1 + maximum(map altura l)

--c)
prune :: Int -> RTree a -> RTree a
prune 1 (R a l) = (R a [])
prune n (R a l) = (R a (map( prune (n-1))l))

--d)
mirror :: RTree a -> RTree a
mirror (R a []) = (R a [])
mirror (R a l) = (R a (reverse(map mirror l)))

--e)
{-}
postorder :: RTree a -> [a]
postorder (R a []) = [a]
postorder (R a l) = postorderAux  ++ [a]
                  where postorderAux = foldr 1 (++) (map postorder l) 
-}

--3
data LTree a = Tip a | Fork (LTree a) (LTree a)
--a)
ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork a b) = ltSum a + ltSum b

--b)
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork a b) = listaLT a ++ listaLT b

--c)
ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1
ltHeight (Fork a b) = 1+ max (ltHeight a) (ltHeight b)


--4)
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
--a)
{-}
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf a) = 
-}
















