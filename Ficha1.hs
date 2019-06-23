import Data.Char
--1
--a)
perimetro::Float->Float
perimetro r = 2*3.14*r

--b)
dist::(Double,Double) -> (Double,Double) -> Double
dist (a1,b1) (a2,b2) = sqrt(((a1-a2)*(a1-a2)) + ((b1-b2)*(b1-b2)))

--c)
primUlt :: [a] -> (a,a)
primUlt (h:t) = (h,last t)

--d)
multiplo::Int -> Int -> Bool
multiplo n m = if mod n m == 0 then True else False

--e)
truncaImpar::[a] -> [a]
truncaImpar [] = []
truncaImpar (h:t) = if odd (length (h:t)) then t else (h:t)

--f)
max2::Int -> Int -> Int
max2 a b= if a>b then a else b

--g)
max3::Int->Int->Int->Int
max3 a b c= if a>b then max2 a c else max2 b c

--2
--a)


--b)


--3

type Hora = (Int,Int)
--a)
valida::Hora-> Bool
valida (a,b) =  if a>=0 && a<=24 && b>=0 && b<=60 then True else False

--b)
compara::Hora -> Hora -> Bool
compara (a,b) (c,d) = if (a>c) then True else comparaMin b d

comparaMin :: Int -> Int -> Bool
comparaMin b d = if b > d then True else False

--c)
converteHoraMin:: Hora -> Int 
converteHoraMin (a,b) = a*60 + b

--d)
converteMinHora::Int->Hora
converteMinHora a = (div a 60, mod a 60)

--e)
diferenca::Hora->Hora-> Int
diferenca (a,b) (c,d) = (converteHoraMin (a-c,b-d))

--f)
adiciona:: Int -> Hora -> Hora
adiciona n (a,b) = converteMinHora (n+(converteHoraMin (a,b)))

--4
data Hora1 = H Int Int 
          deriving (Show,Eq)

--a)
valida'::Hora1-> Bool
valida' (H a b) =  a>=0 && a<=24 && b>=0 && b<=60

---5)
data Semaforo = Verde | Amarelo | Vermelho 
              deriving (Show,Eq)

--a)
next::Semaforo->Semaforo 
next a | a==Verde    =Amarelo
       | a==Amarelo  =Vermelho
       | a==Vermelho =Verde

--b)
stop :: Semaforo -> Bool
stop a = if a==Vermelho then True else False

--c)
safe :: Semaforo -> Semaforo -> Bool
safe a b = if (a==Verde && b==Vermelho) || (a==Vermelho && b==Verde) then True else False

--6
data Ponto = Cartesiano Double Double | Polar Double Double
           deriving (Show,Eq)

--a)
posx :: Ponto -> Double
posx (Cartesiano a b) = abs(a)

--b)
posy :: Ponto -> Double
posy (Cartesiano a b) = abs(b)

--c)
raio :: Ponto -> Double
raio (Cartesiano a b) = sqrt (a*a + b*b)

--d)
angulo :: Ponto -> Double
angulo (Polar a b ) = abs(b)

--e)
dist1 :: Ponto -> Ponto -> Double
dist1 (Cartesiano a b) (Cartesiano c d) = sqrt(((a-c)*(a-c))+((d-b)*(d-b)))

--7
data Figura = Circulo Ponto Double
              | Rectangulo Ponto Ponto
              | Triangulo Ponto Ponto Ponto
                deriving (Show,Eq)
--a)

--b)
vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Rectangulo (Cartesiano a b) (Cartesiano c d))  = [(Cartesiano a b),(Cartesiano a d),(Cartesiano c d),(Cartesiano c b)]
vertices (Triangulo (Cartesiano a b) (Cartesiano c d) (Cartesiano e f)) = [(Cartesiano a b),(Cartesiano c d), (Cartesiano e f)]

--c)
{-}
area :: Figura -> Double
       area (Triangulo p1 p2 p3) =
              let a = dist p1 p2
                  b = dist p2 p3
                  c = dist p3 p1
                  s = (a+b+c) / 2 -- semi-perimetro
              in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
-}

--8
--a)
isLower :: Char -> Bool
isLower a = (a >='a' && a<='z') 

--b)
isDigit :: Char -> Bool
isDigit a = a>='0' && a<='9'

--c)
isAlpha :: Char -> Bool
isAlpha a = ((a >='a' && a<='z') || (a>='A' && a<='Z'))

--d)
toUpper' :: Char -> Char
toUpper' a = (toEnum ((fromEnum a)+32))

































































