data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show

--1)
t3::BTree Int
t3 = Node 7 (Node 5(Node 3 Empty Empty) (Node 6 Empty Empty)) (Node 15 (Node 8 Empty Empty) Empty)

--a)
altura :: BTree a -> Int
altura Empty = 0
altura (Node a e d) = 1 + max (altura e) (altura d)

--b)
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node a e d) = 1 + contaNodos e + contaNodos d

--c)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node a Empty Empty) = 1
folhas (Node a e d) = folhas e + folhas d

--d)
prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune n (Node a e d) = (Node a (prune (n-1) e) (prune (n-1) d))

--e)
path :: [Bool] -> BTree a -> [a]
path [] _ = []
--path [s] (Node a e d) = a
path (h:t) (Node a e d) = if h==False then a:(path t e) else a:(path t d)

--f)
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node a e d) = (Node a (mirror d) (mirror e))

--g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty _ = Empty
zipWithBT f _ Empty = Empty
zipWithBT f (Node a e d) (Node a1 e1 d1) = (Node (f a a1) (zipWithBT f e e1) (zipWithBT f d d1))

--h)
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (x,y,z) e d) = let (a1,b1,c1) = (unzipBT e)
                                 (a2,b2,c2) = (unzipBT d)
                              in (Node x a1 a2,Node y b1 b2, Node z c1 c2)
{-
unzipBT (Node (x,y,z) e d) = (a1,a2,a3)
                           where a1= (Node x (unzipBT e) (unzipBT d))
                                 a2= (Node y (unzipBT e) (unzipBT d))
                                 a3= (Node z (unzipBT e) (unzipBT d))
-}

--2
--a)
minimo :: Ord a => BTree a -> a
minimo (Node a Empty _) = a
minimo (Node a e _) = minimo e

--b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node a Empty Empty) = Empty
semMinimo (Node a Empty d) = d
semMinimo (Node a e d) = (Node a (semMinimo e) d)

--c
--minSmin :: Ord a => BTree a -> (a,BTree a)
--minSmin (Node a e d) = if (a== (minimo (Node a e d))) then (a,d) else (Node a (minSmin e) d)


--3)
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL  deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
                   deriving Show
type Turma = BTree Aluno

t2::Turma
t2 = Node (5,"joao",ORD,Aprov 12) (Node(3,"Ana",TE,Aprov 13)(Node(2,"carol",ORD,Aprov 18) Empty Empty)
                                               (Node(7,"Rui",ORD,Rep) Empty Empty))
                           (Node(10,"Ze",ORD,Aprov 11)(Node(4,"manel",ORD,Faltou) Empty Empty)
                                                 Empty)
--a)
inscNum :: Numero -> Turma -> Bool
inscNum n Empty = False
inscNum n ( Node (num,nom,reg,classe) e d) = if n==num then True else inscNum n e || inscNum n d

--b)
inscNome :: Nome -> Turma -> Bool
inscNome n Empty = False
inscNome n (Node (num,nom,reg,classe) e d) = if n==nom then True else inscNome n e || inscNome n d

--c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nom,reg,classe) e d) = [(num,nom)] ++ trabEst e ++ trabEst d

--d)
nota :: Numero -> Turma -> Maybe Classificacao
nota n Empty = Nothing
nota n (Node (num,nom,reg,classe) e d) |n==num = Just classe 
                                       |n<num = nota n e
                                       |otherwise = nota n d


--e)
percFaltas :: Turma -> Float
percFaltas (Node (num,nom,reg,classe) e d) = ((fromIntegral (percFaltasAux (Node (num,nom,reg,classe) e d)))/(fromIntegral (contaNodos (Node (num,nom,reg,classe) e d))))  * (fromIntegral 100)

percFaltasAux ::  Turma  -> Int
percFaltasAux Empty = 0
percFaltasAux (Node (num, nom, reg, Faltou) e d) = 1+percFaltasAux e + percFaltasAux d
percFaltasAux (Node (num,nom,reg,_) e d) = percFaltasAux e + percFaltasAux d

--f)

somaAprov :: Turma -> Int
somaAprov Empty =  0 
somaAprov (Node (num,nom,reg,Rep) e d) =  somaAprov e + somaAprov d
somaAprov (Node (num,nom,reg,Faltou) e d) =  somaAprov e + somaAprov d
somaAprov (Node (num,nom,reg,classe) e d) =  (obtemNota classe) + somaAprov e + somaAprov d


mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov l = (fromIntegral (somaAprov l)) / (fromIntegral (numeroAprov l)) 

--fromIntegral (fromIntegral((obtemNota classe)) + (mediaAprov e) + (mediaAprov d)))/ (fromIntegral ((numeroAprov (Node (num,nom,reg,classe) e d) )))
obtemNota :: Classificacao -> Int
obtemNota (Aprov x) =  x

numeroAprov:: Turma -> Int
numeroAprov Empty = 0
numeroAprov (Node (num,nom,reg,Rep) e d) = 0
numeroAprov (Node (num,nom,reg,Faltou) e d) = 0
numeroAprov (Node (num,nom,reg,classe) e d) = 1 +numeroAprov e + numeroAprov d






































