--1
--a)
myany :: (a -> Bool) -> [a] -> Bool
myany f [] = False
myany f (h:t) = if f h then True else myany f t

--b)
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f l [] = []
zipWith' f [] l = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

--c)
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = [] 
takeWhile' f (h:t) = if f h then h:takeWhile' f t else []

--d)
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (h:t) = if f h then dropWhile' f t else (h:t)

--e)
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (h:t) | f h = (h:a,b)
              | otherwise = ([],(h:b)) 
              where (a,b) = span' f t

--f)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f a [] = []
deleteBy' f a (h:t) = if f a h then t else h:deleteBy' f a t

--g)
{-}
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f [a] = [a]
sortOn' f (h:x:t) |(f h) <= (f x) then 
-}

--2

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau x [] = []
selgrau x l =  filter f l
                    where f (a,b) = x==b
--b)
conta :: Int -> Polinomio -> Int
conta x l = foldr f 0 l
          where f (a,b) r= if x==b then r+1 else  r

--c)
grau :: Polinomio -> Int
grau l = foldr f 0 l
       where f (a,b) r = if b>r then b else r

--d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv l = map f l
        where f (a,b) = (a*(fromIntegral b),b-1)


--e)
calcula :: Float -> Polinomio -> Float 
calcula x l = foldr f 0 l
             where f (a,b) r = r+(a*(x^b))

--f)
simp :: Polinomio -> Polinomio
simp [] = [] 
simp l = filter f l
       where f (a,b) = a/=0

--g)
mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) l = map f l
              where f (a,b) = (a*x,y+b)

--h)
{-}
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((a,b):t) = map f ((a,b):t)
                 where f (a,b) = insere (a,b) t
	--(insere (a,b) (ordena t))

insere :: Monomio -> Polinomio  -> Polinomio
insere (a,b) [] = [(a,b)]
insere (a,b) ((c,d):t) = if b<=d then ((a,b):(c,d):t) else (c,d): (insere (a,b) t)
-}
--i)
--normaliza :: Polinomio -> Polinomio
--normaliza [] = []
--normaliza l = filter f l 
--        where f (a,b) = 


--3)
type Mat a = [[a]]

--a)
dimOK :: Mat a -> Bool
dimOK [a] = True
dimOK (x:y:t) = if (length x) == (length y) then dimOK (y:t) else False

--b)
dimMat :: Mat a -> (Int,Int)
dimMat ((x:xs):t) = (length (((x:xs):t)), length (x:xs))

--c)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (x:xs) (y:ys) = (somaLista  x y) : addMat xs ys

somaLista:: Num a => [a] -> [a] -> [a]
somaLista [] [] = []
somaLista (x:xs) (y:ys) = (y+x) :somaLista xs ys

{-}
--d)
transpose :: Mat a -> Mat a
transpose [] = []
transpose ((x:xs):(y:ys):t) 
-}






























