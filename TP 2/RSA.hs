module RSA where
import Tipos
import Aritmetica


--(4)
claves :: Integer -> Integer -> (Integer, Integer, Integer)
claves p q = (e, d, n)
  where m = (p-1) * (q-1)
        n = p * q
        e = coprimoCon m
        d = inversoMultiplicativo e m


--(5)
codificador :: Clpub -> Mensaje -> Cifrado
codificador (e,n) [] = []
codificador (e,n) l | mcdEuclides h n == 1 = (modExp h e n) : codificador (e,n) (tail l)
                    | otherwise = -h : codificador (e,n) (tail l)
  where h = head (aEnteros l)

--(6)
decodificador :: Clpri -> Cifrado -> Mensaje
decodificador (d,n) [] = ""
decodificador (d,n) c | (head c) >= 0 = aChars((modExp (head c) d n) : aEnteros (decodificador (d,n) (tail c)))
                      | otherwise = aChars((-(head c)) : aEnteros (decodificador (d,n) (tail c))) 
