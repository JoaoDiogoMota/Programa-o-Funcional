import Data.Char

--1 -> Feito no caderno (10.10)
 
--2
--a
dobros::[Float]->[Float]
dobros [] = []
dobros (h:t) = h*2:dobros t

--b
numOcorre :: Char -> String -> Int
numOcorre a (h:t) = numOcorreAux 0 a (h:t)

numOcorreAux:: Int -> Char -> String -> Int
numOcorreAux n a [] = n
numOcorreAux n a (h:t) = if (a==h) then numOcorreAux (n+1) a t else numOcorreAux n a t

--c
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if (h>0) then positivos t else False

--d
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if (h>0) then h:soPos t else soPos t


--e
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if (h<0) then h+somaNeg t else somaNeg t

--f
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt [a] = [a]
tresUlt [a,b] = [a,b]
tresUlt [a,b,c] = [a,b,c] 
tresUlt (h:t) = tresUlt t

--g
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t )= b:segundos t

--h
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros n [] = False
nosPrimeiros n ((a,b):t) = if (n==a) then True else nosPrimeiros n t

--i
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ((a,b,c),(d,e,f):t) = sumTriplos ((a+d),(b+e),(c+f):t)


--3
--a
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if (h>='1' && h<='9') then h:soDigitos t else soDigitos t


--b
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) = minusculasAux 0 (h:t)

minusculasAux :: Int -> [Char] -> Int
minusculasAux n [] = n 
minusculasAux n (h:t) = if (h>='a' && h<='z') then minusculasAux (n+1) t else minusculasAux n t

--c
nums :: String -> [Int]
nums [] = [] 
nums (h:t) = if (h>='0' && h<='9') then (digitToInt h): (nums t) else nums t

--4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a
conta :: Int -> Polinomio -> Int
conta n ((a,b):t) = contaAux 0 n ((a,b):t)

contaAux :: Int -> Int -> Polinomio -> Int
contaAux x n ((a,b):t) = if (n==b) then contaAux (x+1) n t else contaAux x n t

--b
grau :: Polinomio -> Int
grau [(a,b)] = b
grau ((a,b):(c,d):t) = if (b>=d) then grau ((a,b):t) else grau ((c,d):t)

--c
selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n ((a,b):t) = if (n==b) then (a,b): (selgrau n t) else selgrau n t

--d
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((a,b):t) = ((a*(fromIntegral b)),(b-1)): deriv t

--e
calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x ((a,b):t) = a * (x^(fromIntegral b)) + calcula x t

--f
simp :: Polinomio -> Polinomio
simp [] = []
simp ((a,b):t) = if (a==0) then simp t else (a,b):simp t

--g
mult :: Monomio -> Polinomio -> Polinomio
mult l [] = []
mult (a,b) ((c,d):t) = if (a==c) then (a,b+d):(mult (a,b) t) else (c,d):mult (a,b) t

--h
normaliza :: Polinomio -> Polinomio
normaliza ((a,b),(c,d):t) = if (b==d) then (normalizaAux (((a+c),d):t)) else normalizaAux 

normalizaAux::Polinomio->Polinomio
normalizaAux ((a,b),(c,d):t) = if (b==d) then (normalizaAux (((a+c),b):t)) else (normalizaAux(a,b):t)






























