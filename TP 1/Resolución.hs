
--- Ejercicio 1

esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos n = esSumaAux n (round (fromIntegral n**(1/3))) 1

esSumaAux :: Integer -> Integer -> Integer -> Bool
esSumaAux m x y | x==0 = False
   |  x^3 + y^3 == m = True
   |  x^3 +  y^3 < m = esSumaAux m x (y+1)
   |  x^3 +  y^3 > m = esSumaAux m (x-1) y



--- Ejercicio 2

descomposicionCubos :: Integer -> (Integer,Integer)
descomposicionCubos n = primerPar  n (round(fromIntegral n**(1/3))) 1

primerPar :: Integer -> Integer -> Integer -> (Integer,Integer)
primerPar  m x y | x == 0 = (0,0)
   | (x^3) + (y^3) == m = (x,y)
   |(x^3) + (y^3) < m = primerPar m x (y+1)
   |(x^3) + (y^3) > m = primerPar  m (x-1) y



--- Ejercicio 3

cantidadDeFormas :: Integer ->Integer
cantidadDeFormas n = contador n (descomposicionCubos n) 0

siguientePar :: Integer -> (Integer, Integer) -> (Integer, Integer)
siguientePar n (x , y) = primerPar n (x-1) y

contador :: Integer -> (Integer, Integer) -> Integer -> Integer
contador n (x, y) c | x == 0 = round(fromIntegral c/2) -- dividimos por dos porque sino cuenta (12,1) y (1,12)
   | x == y = 1
   |otherwise = contador n (siguientePar n (x, y)) (c+1)

-- el contador tomara el primer par y el 0 como starting point 

--- Ejercicio 4

especialDesde :: Integer -> Integer
especialDesde n | esEspecial n = n
    |otherwise = especialDesde (n+1)

esEspecial :: Integer -> Bool
esEspecial n = cantidadDeFormas n >= 2


--- Ejercicio 5 

especialNumero :: Integer -> Integer
especialNumero 1 = 1729
especialNumero n = especialDesde ( 1 + especialNumero (n-1))


--- Ejercicio 6

esMuyEspecial :: Integer -> Bool
esMuyEspecial n | not (esEspecial n) = False
  |otherwise = condiciones n 2

esUnCubo :: Integer -> Bool
esUnCubo x = round (fromIntegral x ** (1/3)) ^ 3 == x

condiciones :: Integer -> Integer -> Bool
condiciones n k | k == round (fromIntegral n ** (1/3)) = True 
        | mod n k == 0 && esUnCubo k && esEspecial (div n k) = False
        | mod n k == 0 && esUnCubo k && not (esEspecial (div n k)) = True
        | otherwise = condiciones n (k+1)

