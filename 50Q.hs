--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y = if x<=y then x:enumFromTo' (x+1) y else []

--2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z = if x<=z then  x: (enumFromThenTo' y (y+(y-x)) z) else []

--3
(+++):: [a]->[a]->[a]
(+++) [] l = l
(+++) l [] = l
(+++) l a = l++a

--4
(!!!) :: [a] -> Int -> a
(!!!) (h:t) x =  if x==0 then h else (!!!) (h:t) (x-1)

--5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' l = last l : (reverse' (init l))

--6
take' :: Int -> [a] -> [a]
take' 0 l = l
take' x [] = []
take' x (h:t) = h:(take (x-1) t)

--7
drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' x [] = []
drop' x (h:t) = drop' (x-1) t

--8
zip' :: [a] -> [b] -> [(a,b)]
zip' [] l = []
zip' l [] = [] 
zip' (x:xs) (y:ys) = (x,y): zip' xs ys

--9
elem' :: Eq a => a -> [a] ->Bool
elem' x [] = False
elem' x (h:t) = if x==h then True else elem' x t

--10
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' x a = a:(replicate' (x-1) a)

--11
intersperse' :: a -> [a] -> [a]
intersperse' x [a] = [a]
intersperse' x (h:t) = h:x: intersperse' x t

--12
{-}
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = if last t == last (init (h:t)) then group' (init (h:t)) : last (h:t) else 
-}

--13
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ (concat' t)

--14
inits':: [a]->[[a]]
inits' l = initsAux 0 l
 
initsAux :: Int-> [a] -> [[a]]
initsAux n l = if (n<=(length l))  then take n l : (initsAux (n+1) l) else []

--15
tails':: [a]->[[a]]
tails' [] = [[]]
tails' (h:t) = (h:t) : tails' (init (h:t))

--16
isPrefixOf':: Eq a =>[a]-> [a] -> Bool
isPrefixOf' l [] = False
isPrefixOf' [] l = True
isPrefixOf' (x:xs) (y:ys) = if (x==y) then isPrefixOf' xs ys else False 

--17
isSuffixOf':: Eq a =>[a] -> [a] -> Bool
isSuffixOf' [] l = True
isSuffixOf' l [] = False
isSuffixOf' x y = if (last x) == (last y) then isSuffixOf' (init x) (init y) else False

--18
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] l = True
isSubsequenceOf' l [] = False
isSubsequenceOf' (x:xs) (y:ys) = if (x==y) then isSubsequenceOf' xs ys else isSubsequenceOf' (x:xs) ys

--19
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' x [] = []
elemIndices' x (h:t) = (dizPosicao x (h:t)) : (elemIndices' x t)

dizPosicao:: Eq a => a -> [a] -> Int
dizPosicao x (h:t) = dizPosicaoAux x (h:t) 0

dizPosicaoAux:: Eq a=> a -> [a] -> Int -> Int 
dizPosicaoAux x (h:t) a = if (h==x) then a else (dizPosicaoAux x t (a+1))

--20

nub' :: Eq a => [a] -> [a]
nub' l = reverse (nubAux' l)


nubAux' :: Eq a => [a] -> [a]
nubAux' []  = []
nubAux' [a] = [a]
nubAux' (h:t) = if elem h t then nub' t else h:(nub' t)

--21
delete' :: Eq a => a -> [a] -> [a]
delete' x [] = []
delete' x (h:t) = if (x==h) then t else h:(delete' x t)

--22 V
(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) [] l = []
(\\\) l [] = l
(\\\) (x:xs) (y:ys) = ((\\\) (delete' y (x:xs)) ys)

--23
union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l
union' [] l = l
union' l (y:ys) = if (elem y l) then union' l ys else union' (l++[y]) ys

--24
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] l = []
intersect' l [] = []
intersect' (x:xs) l = if elem x l then x:intersect' xs l else intersect' xs l

--25
insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (h:t) = if (a<=h) then (a:h:t) else h:insert' a t

--26
unwords' :: [String] -> String
unwords' [a] = a
unwords' (h:t) = h ++ " " ++ unwords' t

--27
unlines' :: [String] -> String
unlines' [a] = a ++ "\n"
unlines' (h:t) = h++ "\n" ++ (unlines' t)

--Ver como fazer
--28
pMaior :: Ord a => [a] -> Int
pMaior l = onde (pMaiorAux l) l 0

pMaiorAux :: Ord a =>  [a] -> a
pMaiorAux [a] = a
pMaiorAux (h:y:t) = if h>y then pMaiorAux (h:t) else pMaiorAux (y:t)

onde::Eq a =>  a -> [a] -> Int -> Int
onde x (h:t) n = if (x==h) then n else onde x t (n+1)

--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) = if elem h t then True else temRepetidos t

--30
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) = if (h>='0' && h<='9') then h:algarismos t else algarismos t 

--31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (h:y:t) = y:posImpares t

--32
posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (x:y:t) = x: posPares t

--33
isSorted :: Ord a => [a] -> Bool
isSorted [a] = True
isSorted [] = True
isSorted (h:y:t) = if y>=h then isSorted (y:t) else False

--34
iSort :: Ord a => [a] -> [a]
iSort [x] = [x]
iSort [] = []
iSort (h:t) = (insert' h (iSort t))

--35
menor :: String -> String -> Bool
menor [] l = True
menor (x:xs) (y:ys) = if x==y then menor xs ys else False

--36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((x,y):t) = if a==x then True else elemMSet a t

--37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((a,b):t) = b+lengthMSet t

--38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):t) = (replicate b a) ++ converteMSet t

--39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,b):t) = if x==a then (a,b+1):t else (a,b) :insereMSet x t

--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = [(x,1)]
removeMSet x ((a,b):t) | x==a && b==1 = t
                       | x==a && b>1 = (a,(b-1)) :t
                       | otherwise = (a,b) : removeMSet x t

--41
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = (insereMSet h (constroiMSet t))


--42
partitionEithers' :: [Either a b] ->([a],[b])
partitionEithers' l = (leftAux l, rightAux l)

leftAux:: [Either a b] -> [a]
leftAux [] = []
leftAux (Left a : t) = a: (leftAux t)
leftAux (_:t) = leftAux t

rightAux :: [Either a b] -> [b]
rightAux [] = []
rightAux (Right a : t) = a: (rightAux t)
rightAux (_:t) = rightAux t

--43
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a:t) = a:catMaybes t
catMaybes (Nothing:t) = catMaybes t

data Movimento = Norte | Sul | Este | Oeste
               deriving Show

--44
posicao:: (Int,Int)->[Movimento]->(Int,Int)
posicao (a,b) [] = (a,b)
posicao (a,b) (h:t) = (a + este (h:t) - oeste (h:t),b+ norte (h:t) - sul (h:t))
                    where norte (Norte:t) = 1 + norte t
                          norte (_:t) = norte t
                          norte [] = 0
                          sul (Sul:t) = 1 + sul t
                          sul (_:t) = sul t
                          sul [] = 0
                          oeste (Oeste:t) = 1+oeste t
                          oeste(_:t) = oeste t
                          oeste [] =0 
                          este (Este:t) = 1+este t
                          este(_:t) = este t
                          este [] =0 
--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (a,b) (x,y) | a==x && b==y = []
                    | a==x && b>y = Sul:caminho(a,b-1) (x,y)
                    | a==x && b<y = Norte:caminho(a,b+1) (x,y)
                    | a>x && b<y = Oeste:caminho (a-1,b) (x,y)
                    | a>x && b>y = Oeste:caminho (a-1,b) (x,y)
                    | a<x && b<y = Este: caminho (a+1,b) (x,y)
                    | otherwise = Este :caminho (a+1,b) (x,y)

--46
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (Norte:t) = vertical t
vertical (Sul:t) = vertical t
vertical (_:t) = False


data Posicao = Pos Int Int
            deriving Show

--47
--maisCentral :: [Posicao] -> Posicao
--maisCentral [Pos a b] = Pos a b
--maisCentral (Pos a b:Pos x y :t) = if calcula (Pos a b) < calcula (Pos x y) then maisCentral ((Pos a b):t) else maisCentral ((Pos x y):t)

--calcula:: Int -> Int -> Double
--calcula a b  = (sqrt((a*a)+(b*b)))


--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada (Pos a b:Pos x y:t) = if(a==x) then mesmaOrdenada (Pos x y:t)else False

--50

data Semaforo = Verde | Amarelo | Vermelho
              deriving Show

interseccaoOK :: [Semaforo] -> Bool 
interseccaoOK [] = True
interseccaoOK (h:t) | contaVerde (h:t) > 1 = False
                    |otherwise = True
                    where contaVerde []  = 0
                          contaVerde (Verde:t)  = 1 + contaVerde t
                          contaVerde (_:t) = contaVerde t
 











