module Aritmetica where
import Tipos
import Data.Tuple
import Data.Bits

-- Función de regalo para exponenciar "rápido"
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1

-- --(?)
-- mcdExt :: Integer -> Integer -> (Integer, (Integer, Integer))
-- mcdExt _ _ = (0, (0, 0))


--(1)

menorDivAux ::  Integer -> Integer -> Integer
menorDivAux n k | mod n k == 0 = k
                | otherwise = menorDivAux n (k+1)

menorDiv :: Integer -> Integer
menorDiv n  = menorDivAux n 2

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = n == menorDiv n

criba :: Integer -> Set Integer
criba 1 = []
criba n | esPrimo (n-1) = (n-1) : criba (n-1)
        | otherwise = criba (n-1)
 
--(2)
modulo::Integer->Integer
modulo a | a >= 0 = a 
         | a < 0 = -a

mcdEuclides::Integer-> Integer -> Integer 
mcdEuclides a 0 = modulo a
mcdEuclides a b = mcdEuclides b (mod a b)


coprimoConAux:: Integer -> Integer -> Integer 
coprimoConAux n m | m == (n-1) = undefined
                  | mcdEuclides n m == 1 = m
                  | otherwise = coprimoConAux n (m+1) 

coprimoCon:: Integer -> Integer
coprimoCon n = coprimoConAux n 2 


--(3)

inversoMultiplicativoAux:: Integer -> Integer -> Integer -> Integer
inversoMultiplicativoAux n m k | mcdEuclides n m /= 1 = undefined
                               | mod (n*k) m == 1 = k
                               | otherwise = inversoMultiplicativoAux n m (k+1)

inversoMultiplicativo:: Integer -> Integer -> Integer
inversoMultiplicativo n m = inversoMultiplicativoAux n m 1 



