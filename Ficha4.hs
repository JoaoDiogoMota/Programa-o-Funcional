import Data.Char
--3
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t) |isDigit h = (h:ld,la)
                 |isAlpha h = (ld,h:la)
                 |otherwise = (ld,la)
           
                 where (ld,la) = digitAlpha t

--4)

nzip :: [Int] -> (Int,Int,Int)
nzip [] = (0,0,0)
nzip (h:t) | h<0 = (x+1,y,z)
           | h==0 = (x,y+1,z)
           | otherwise = (x,y,z+1) 
            where (x,y,z) = nzip t


--5)
divMod' :: Integral a => a -> a -> (a, a)
divMod' n x = primAux n x 0 

primAux:: Integral a => a -> a -> a -> (a,a)
primAux n x cont = if (n-x) > 0 then primAux (n-x) x (cont+1) else divModAux n x cont

divModAux :: Integral a => a -> a -> a -> (a,a)
divModAux n x cont = if (n-x) == 0 then (cont,0) else (cont,n)


--6)
fromDigits :: [Int] -> Int
fromDigits []    = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t

--7
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l]