import Data.List

data Cores = Vermelho Int
           | Verde Int
           | Azul Int
           

e::Cores
e = Vermelho 20
v = Verde 80
a = Azul 50

lc::[Cores]
lc = [e,v,a]


--testa se duas cores sao iguais endependentemente da sua intensidade 
coresIguais :: Cores -> Cores -> Bool
coresIguais (Verde _) (Verde _ ) = True
coresIguais (Vermelho _) (Vermelho _) = True
coresIguais (Azul _ ) (Azul _) = True
coresIguais _ _  = False

instance Eq Cores where
                 (==) = coresIguais 



--Compara a intensidade das cores
comparaCores :: Cores -> Cores -> Bool
comparaCores c1 c2 = intensidade c1 > intensidade c2


intensidade :: Cores -> Int
intensidade (Azul i) = i
intensidade (Vermelho i) = i
intensidade (Verde i) = i

instance  Ord Cores where
    (<=) a b= comparaCores b a

showCores :: Cores -> String
showCores (Vermelho i) |i<=20 = "Vermelho Claro"
                       | i>75 = "Vermelho Escuro"
                       |otherwise = "Vermelho"
showCores (Azul i) |i<=20 = "Azul Claro"
                   | i>75 = "Azul Escuro"
                   |otherwise = "Azul"
showCores (Verde i) |i<=20 = "Verde Claro"
                    | i>75 = "Verde Escuro"
                    |otherwise = "Verde"

instance Show Cores where
    show = showCores


--2
data Exp a = Const a
             | Simetrico (Exp a)
             | Mais (Exp a) (Exp a)
             | Menos (Exp a) (Exp a)
             | Mult (Exp a) (Exp a)
             

expr::Exp Int
expr = ((Const 2) `Mais`(Const 3)) `Mult` (Const 5)

expr'''::Exp Int
expr'''= ((Const 3) `Mais`(Const 2)) `Mult` (Const 5)

expr'::Exp Float
expr' = Const 2.2

--a)
calcula :: Num a => Exp a -> a 
calcula (Const x) = x
calcula (Simetrico e) = -(calcula e)
calcula (Mais x y) = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Mult x y) = (calcula x) * (calcula y)

--a)
infixa :: Show a => Exp a -> String
infixa (Const x) = show x
infixa (Simetrico e) = "-(" ++ infixa e++")"
infixa (Mais x y) = (infixa x) ++ "+" ++ (infixa y)
infixa (Menos x y) = (infixa x) ++ "-" ++ (infixa y) 
infixa (Mult x y) = "(" ++ (infixa x) ++ "*" ++ (infixa y) ++ ")"

instance Show a => Show (Exp a) where
        show = infixa

--b)
--qquero que (==) de True se o resultado do calcula é igual
igualExp::(Num a, Eq a) => Exp a -> Exp a -> Bool
igualExp e1 e2 = calcula e1 == calcula e2

instance (Num a,Eq a) => Eq (Exp a) where
    (==) e1 e2 = igualExp e1 e2

--c)
-- Num : +,-,*,abs.signum, fromInteger

somaExp :: Num a => Exp a -> Exp a -> Exp a
somaExp e1 e2 = Const ((calcula e1) +(calcula e2))

subExp :: Num a => Exp a -> Exp a -> Exp a
subExp e1 e2 = Const ((calcula e1) -(calcula e2))

multExp :: Num a =>Exp a -> Exp a -> Exp a
multExp e1 e2 = Const ((calcula e1) *(calcula e1))

--abs x * signum x = x

absExp :: Num a => Exp a -> Exp a
absExp e = Const (abs(calcula e))
          

signumExp ::Num a => Exp a -> Exp a
signumExp e = Const (signum(calcula e))
























































