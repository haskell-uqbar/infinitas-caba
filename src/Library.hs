module Library where
import PdePreludat
import Test.Hspec (xcontext)

doble :: Number -> Number
doble numero = numero + numero


factorial :: Number -> Number
factorial 0 = 1
factorial n = n * factorial (n-1)

f [] = 0
f [x] = x
f (cabeza:(2:_)) = cabeza * 10
f (_:(segundaCabeza:_)) = segundaCabeza




cantidadElementos :: [a] -> Number
cantidadElementos [] = 0
cantidadElementos (_:xs) = 1 + cantidadElementos xs

f1 :: [a] -> a
f1 [x] = x
f1 (_:xs) = f1 xs

--sumatoria
f2 :: [Number] -> Number
f2 [] = 0
f2 (x:xs) = x + f2 xs

f2' lista = foldl (+) 0 lista

-- multiplica a todos por 56
f3 :: [Number] -> [Number]
f3 [] = []
f3 (x:xs) = x * 56 : f3 xs

--f3  = f4 56 

--multiplica a todos por el nro dado
f4 :: Number -> [Number] -> [Number]
--f4 n [] = []
--f4 n (x:xs) = x * n : f4 n xs


f4 n = f5 (* n) 

--map
f5 :: (a -> b) -> [a] -> [b]
f5 f [] = []
f5 f (x:xs) = f x : f5 f xs

-- any
f6 :: (t -> Bool) -> [t] -> Bool
f6 f [] = False
f6 f (x:xs) = f x || f6 f xs

-- all
f6' :: (t -> Bool) -> [t] -> Bool
f6' f [] = True
f6' f (x:xs) = f x && f6' f xs

otra :: (Number, Bool) -> Bool
otra (3,True) = True

cualquiera :: String -> Number
cualquiera "hola" = 89


-- Fold

data Ciudad = Ciudad {
    frase:: String,
    limpieza:: Number
} deriving Show

esLimpia :: Ciudad -> Bool
esLimpia (Ciudad f l) = l > 8

limpiar :: Number -> Ciudad -> Ciudad
limpiar n ciudad = ciudad{limpieza = limpieza ciudad * n}

limpiarAlReves ::  Ciudad -> Number -> Ciudad
limpiarAlReves ciudad n = ciudad{limpieza = limpieza ciudad * n}


-- limpia la ciudad con valor 3 y luego 2
limpiarRepetido :: Ciudad -> Ciudad
limpiarRepetido ciudad = limpiar 2 (limpiar 3 ciudad)

-- limpia la ciudad tambien dos veces, pero con valores variables 
limpiarRepetidoVariable :: Number -> Number -> Ciudad -> Ciudad
--limpiarRepetidoVariable v1 v2 ciudad = limpiar v2 (limpiar v1 ciudad)
limpiarRepetidoVariable v1 v2 = limpiar v2. limpiar v1


limpiarSucesivamente:: [Number] -> Ciudad -> Ciudad
limpiarSucesivamente valores ciudad = foldr limpiar ciudad valores

f9 _ e [] = e
f9 f e (x:xs) = f x (f9 f e xs)

f10 f e [] = e
f10 f e (x:xs) = f10 f (f e x) xs 


cordoba  = Ciudad "alfajores ricos" 1
bsas = Ciudad "viva bs as" 4 
-- Evaluacion diferida
-- ejemplos de consola:

-- take 3 ("hola" ++ "que tal")
-- "hol"

-- [2*45, 424524124*23112342134,factorial 100000000, 5]!!1
-- 9811746798024640616

-- Listas infinitas
f7 :: Number -> [Number]
f7 x = x : f7 (x+1)

-- (f7 3) !! 4
-- 7

-- (f7 3) !! 40000
-- 40003

-- take 100 (f7 3)
-- [3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102]











data Item = Algo {
    valor:: Number,
    siguiente:: Item
} | Nada


ultimo = Algo 25 Nada
intermedio = Algo 40 ultimo

primero = Algo 100 intermedio

otroPrimero = Algo 200 intermedio

suma:: Item -> Number
suma Nada = 0
suma (Algo x sig) = x + suma sig



data Nodo = Nulo | Nodo {
    elemento :: Number,
    izq:: Nodo,
    der:: Nodo
} 


arbolito = Nodo 6 (Nodo 1 Nulo Nulo) Nulo

arbol1 = Nodo 2 (Nodo 3 Nulo Nulo) arbolito