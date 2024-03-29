module Lab01 where

import Data.List
import Data.Char

{-
1) Corregir los siguientes programas de modo que sean aceptados por GHCi.
-}

-- a)
not2 b = case b of
    True -> False
    False -> True

-- b)
in2 [x]         =  []
in2 (x:xs)      =  x : in2 xs
in2 []          =  error "empty list"

-- c)
length2 []        =  0
length2 (_:l)     =  1 + length2 l

-- d)
list123 = 1 : 2 : 3 : []

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f)
addToTail x xs = map (+x) $ tail xs

-- g)
listmin xs = head . sort $ xs

-- h) (*)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs

{-
2. Definir las siguie--ntes funciones y determinar su tipo:
-}

--a) five, que dado cualquier valor, devuelve 5
five :: Num t => t1 -> t
five x = 5

--b) apply, que toma una función y un valor, y devuelve el resultado de
--aplicar la función al valor dado
apply :: (t1 -> t) -> t1 -> t
apply f x = f x

--c) ident, la función identidad
ident :: t -> t
ident x = x

--d) first, que toma un par ordenado, y devuelve su primera componente
first :: (t1, t) -> t1
first (x, _) = x

--e) derive, que aproxima la derivada de una función dada en un punto dado
derivate :: Fractional a => (a -> a) -> a -> a
derivate f x = fun / eps where
    fun = f (x + eps) - f (x)
    eps = 0.000000001

--f) sign, la función signo
sign :: (Ord a, Num a, Num t) => a -> t
sign x | x < 0  = -1
       | x == 0 = 0
       | True  = 1

--g) vabs, la función valor absoluto (usando sign y sin usarla)
vabs :: (Ord t, Num t) => t -> t
vabs n | n < 0 = negate n
       | True  = n

vabs' :: (Ord t, Num t) => t -> t
vabs' n = n * sign n

--h) pot, que toma un entero y un número, y devuelve el resultado de
--elevar el segundo a la potencia dada por el primero
pot :: (Eq t1, Num t1, Num t) => t1 -> t -> t
pot 0 x = 1
pot n x = x * pot (n-1) x

--i) xor, el operador de disyunción exclusiva
xor :: Bool -> Bool -> Bool
xor p q = (not $ p && q) && (not $ not p && not q)

--j) max3, que toma tres números enteros y devuelve el máximo entre llos
max3 :: Integral b => b -> b -> b -> b
max3 x y z = max x $ max y z

--k) swap, que toma un par y devuelve el par con sus componentes invertidas
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

{-
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}
biciesto :: Integral a => a -> Bool
biciesto n = mult4 && (not mult100 || mult400) where
    mult4   = n `mod` 4 == 0
    mult100 = n `mod` 100 == 0
    mult400 = n `mod` 400 == 0

{-
4)

Defina un operador infijo *$ que implemente la multiplicación de un
vector por un escalar. Representaremos a los vectores mediante listas
de Haskell. Así, dada una lista ns y un número n, el valor ns *$ n
debe ser igual a la lista ns con todos sus elementos multiplicados por
n. Por ejemplo,

[ 2, 3 ] *$ 5 == [ 10 , 15 ].

El operador *$ debe definirse de manera que la siguiente
expresión sea válida:

-}
(*$) :: Num b => [b] -> b -> [b]
xs *$ n = map (*n) xs
v = [1, 2, 3] *$ 2 *$ 4


{-
5) Definir las siguientes funciones usando listas por comprensión:
-}
--a) 'divisors', que dado un entero positivo 'x' devuelve la
--lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)

--b) 'matches', que dados un entero 'x' y una lista de enteros descarta
--de la lista los elementos distintos a 'x'

--c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
--'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
--donde 0 <= a, b, c, d <= 'n'

--(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
--'xs' sin elementos repetidos
unique :: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [0..], not $ elem x (take i xs)]

{-
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}
scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct xs ys = sum $ map (\(x, y) -> x*y) (zip xs ys)

{-
7) Definir mediante recursión explícita
las siguientes funciones y escribir su tipo más general:
-}
--a) 'suma', que suma todos los elementos de una lista de números
suma :: Num a => [a] -> a
suma [x]    = x
suma (x:xs) = x + suma xs

--b) 'alguno', que devuelve True si algún elemento de una
--lista de valores booleanos es True, y False en caso
--contrario
alguno :: [Bool] -> Bool
alguno []     = False
alguno (x:xs) = x || alguno xs

--c) 'todos', que devuelve True si todos los elementos de
--una lista de valores booleanos son True, y False en caso
--contrario
todos :: [Bool] -> Bool
todos []     = True
todos (x:xs) = x && todos xs

--d) 'codes', que dada una lista de caracteres, devuelve la
--lista de sus ordinales
codes :: [Char] -> [Int]
codes []     = []
codes (c:xs) = ord c : codes xs

--e) 'restos', que calcula la lista de los restos de la
--división de los elementos de una lista de números dada por otro
--número dado
--restos :: Num a => [a] -> a -> [a]
restos []     _ = []
restos (x:xs) n = mod x n : restos xs n

--f) 'cuadrados', que dada una lista de números, devuelva la
--lista de sus cuadrados
cuadrados :: Num a => [a] -> [a]
cuadrados []     = []
cuadrados (x:xs) = x^2 : cuadrados xs

--g) 'longitudes', que dada una lista de listas, devuelve la
--lista de sus longitudes
longitudes :: [[a]] -> [Int]
longitudes []       = []
longitudes (xs:xss) = length xs : longitudes xss

--h) 'orden', que dada una lista de pares de números, devuelve
--la lista de aquellos pares en los que la primera componente es
--menor que el triple de la segunda
orden :: (Ord a, Num a) => [(a,a)] -> [(a,a)]
orden []     = []
orden (p:xs) = if fst p < 3 * snd p then p : orden xs
               else orden xs

--i) 'pares', que dada una lista de enteros, devuelve la lista
--de los elementos pares
pares :: Integral a => [a] -> [a]
pares []     = []
pares (x:xs) = if even x then x : pares xs
               else pares xs

--j) 'letras', que dada una lista de caracteres, devuelve la
--lista de aquellos que son letras (minúsculas o mayúsculas)
letras :: [Char] -> [Char]
letras []     = []
letras (c:xs) = if elem c lowerUpper then c : letras xs
                else letras xs
                where lowerUpper = ['a'..'z'] ++ ['A'..'Z']

--k) 'masDe', que dada una lista de listas 'xss' y un
--número 'n', devuelve la lista de aquellas listas de 'xss'
--con longitud mayor que 'n'
masDe :: [[a]] -> Int -> [[a]]
masDe []       _ = []
masDe (xs:xss) n = if length xs >= n then xs : masDe xss n
                   else masDe xss n
