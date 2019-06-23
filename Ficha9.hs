import System.Random
import Data.Char

--1
--a)
bingo::IO()
bingo=do nl <- acumularNumeros []
         print nl

acumularNumeros::[Int] -> IO [Int]
acumularNumeros l |length l ==10 = return l
                  |otherwise = do v <- randomRIO (1,90)
                                  print v
                                  getChar
                                  let nl = if elem v l 
                                        then l else v:l in acumularNumeros nl
--b)


gerarChave::IO (Int,Int,Int,Int)
gerarChave = do d1 <- randomRIO (0,9)
                d2 <- randomRIO (0,9)
                d3 <- randomRIO (0,9)
                d4 <- randomRIO (0,9)
                return (d1,d2,d3,d4)

introduzDigitos :: IO (Int,Int,Int,Int)
introduzDigitos = do c1 <- getChar
                     c2 <- getChar
                     c3 <- getChar    
                     c4 <- getChar   
                     return(digitToInt c1,digitToInt c2,digitToInt c3,digitToInt c4)         

quantosIguais :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> Int
quantosIguais (c1,c2,c3,c4) (v1,v2,v3,v4) = 
      length (filter (== True) [c1==v1,c2==v2,c3==v3,c4==v4] )

mastermind :: IO ()
mastermind = do cs <- gerarChave 
                print cs
                vs <- introduzDigitos 
                chavesEnc <- joga cs vs 
                print chavesEnc

joga:: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> IO (Int,Int,Int,Int)
joga chaves valores |quantosIguais chaves valores == 4 = return valores
                    |otherwise = do vs <- introduzDigitos
                                    joga chaves vs
                    
