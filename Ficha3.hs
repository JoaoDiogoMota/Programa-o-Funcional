data Hora = H Int Int
          deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

--a
testarEtapa:: Etapa -> Bool
testarEtapa (H h1 m1, H h2 m2)  |(h2>h1) = True
                                |(h1==h2) && (m2>m1) = True
                                |otherwise =  False 

--b
testarViagem:: Viagem -> Bool
testarViagem [e] = testarEtapa e
testarViagem (e1:e2:es) = (testarEtapa e1) &&  (testarEtapa (snd e1, fst e2)) && (testarViagem (e2:es))

--c
horaPartidaChegada:: Viagem -> (Hora, Hora) 
horaPartidaChegada e = (fst(head e), (snd(last(e))))

--d
tempoEfetivo:: Viagem -> Int
tempoEfetivo [] = 0
tempoEfetivo (e:es) = (duracao e) + tempoEfetivo es
    where duracao (h1,h2) = (horaParaMin h2 - horaParaMin h1)


horaParaMin :: Hora-> Int
horaParaMin (H x y) = 60*x + y


minParaHora::Int -> Hora
minParaHora m = H (div m 60) (mod m 60)

--e
{-}
tempoEspera ::Viagem -> Int
tempoEspera v = let (p,c) = horaPartidaChegada v
                    totalViagem = horaParaMin c - horaParaMin p
                in totalViagem - (tempoEfetivo vs)
-}

--tempoEspera :: Viagem -> Int
--tempoEspera v = (horaParaMin (fst(horaPartidaChegada v), (snd(horaPartidaChegada v)) - tempoEfetivo v

type Poligonal = [Ponto]
data Ponto = Cartesiano Double Double | Polar Double Double
           deriving (Show,Eq)

data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
             deriving (Show,Eq)

--a
comprimento :: Poligonal -> Double
comprimento [l] = 0
comprimento (h:x:t) = dist' h x + comprimento (x:t)

dist' :: Ponto -> Ponto -> Double
dist' (Cartesiano x y) (Cartesiano a b) = sqrt((x-a)^2+(y-b)^2)

--b
eFechada :: Poligonal -> Bool
eFechada l = head l == last l

--c

triangula :: Poligonal -> [Figura]
triagula [p1,p2,p3] = []
triagula (p1:p2:p3:ps) = (area p1 p2 p3) : triagula (p1:p3:ps)

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
       let a = dist' p1 p2
           b = dist' p2 p3
           c = dist' p3 p1
           s = (a+b+c) / 2 -- semi-perimetro
       in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron


























