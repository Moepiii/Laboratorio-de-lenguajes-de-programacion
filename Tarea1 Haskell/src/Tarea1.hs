-------------------------------------------------------------------------------
-- Tarea 1 - Laboratorio de Lenguajes de Programación I (CI-3661)
-- Nombre del estudiante: Rafael Antonio Valera Pacheco
-- Carnet: 1611202
-------------------------------------------------------------------------------

module Tarea1 where

import Data.Char (toUpper) 
import Text.Read (readMaybe) 

-------------------------------------------------------------------------------
-- Problema 1: Es Palindromo
-------------------------------------------------------------------------------

esPalindromo :: String -> Bool

-- Caso Base 1: Cadena vacía.
esPalindromo [] = True

-- Caso Base 2: Cadena de un solo carácter.
esPalindromo [_] = True

-- Caso Recursivo: La cadena tiene al menos dos caracteres.
-- Descomponemos la cadena en: (x:xs) donde x es el primer elemento
-- y xs es el resto. Luego, aplicamos la recursión.
esPalindromo (x:xs)
    -- Si el primer elemento (x) es igual al último (last xs).
    | x == last xs = 
        -- Llamamos recursivamente al 'cuerpo' de la lista (xs sin el último).
        esPalindromo (init xs)
        
    -- Si no son iguales, no es un palíndromo.
    | otherwise = 
        False

-------------------------------------------------------------------------------
-- Problema 2: Producto de Elementos Pares en una Lista
-------------------------------------------------------------------------------

productoParesRec :: [Integer] -> Integer

-- Caso Base 1: Lista Vacía
-- Si la lista está vacía, el producto es 1.
productoParesRec [] = 1

-- Caso Recursivo 1: El primer elemento (x) es PAR.
-- Usamos 'mod 2 == 0' para verificar la paridad.
productoParesRec (x:xs)
    | x `mod` 2 == 0 = x * productoParesRec xs
    
-- Caso Recursivo 2: El primer elemento (x) es IMPAR.
-- Simplemente ignoramos el número y continuamos la recursión con el resto de la lista (xs).
    | otherwise      = productoParesRec xs

-------------------------------------------------------------------------------
-- Problema 3: Parseo Condicional con Either
-------------------------------------------------------------------------------

parsearCondicional :: [String] -> [Either String Int]

-- Función auxiliar que procesa una única cadena
procesarCadena :: String -> Either String Int
procesarCadena s =
    case readMaybe s of
        -- Éxito: readMaybe devuelve Just n
        Just n  -> Right n
        
        -- Fallo: readMaybe devuelve Nothing
        Nothing -> Left (map toUpper s)

-- Caso Base: Lista vacía.
parsearCondicional [] = []
-- Caso Recursivo: Procesar la cabeza (x) y concatenar con la recursión de la cola (xs).
parsearCondicional (x:xs) = procesarCadena x : parsearCondicional xs

-------------------------------------------------------------------------------
-- Problema 4: Suma Acumulada Condicional
-------------------------------------------------------------------------------

sumaAcumuladaCondicional :: Float -> [Float] -> Float

-- La función recibe el umbral (u) y la lista (l)
sumaAcumuladaCondicional u l = 
    -- Paso 1: Filtrar la lista
    let filtrados = filter (\x -> x > u) l
    
    -- Paso 2: Plegar (Fold) para sumar los elementos filtrados
    in foldr (+) 0 filtrados

-- Explicación del Fold:
-- foldr (+) 0 filtrados
--  - (+) es la función binaria (suma) que se aplica a cada elemento.
--  - 0 es el valor inicial (el elemento neutro de la suma).
--  - filtrados es la lista a plegar.

-------------------------------------------------------------------------------
-- Problema 5: Generación de Coordenadas Impares
-------------------------------------------------------------------------------

coordenadasImpares :: Int -> [(Int, Int)]

-- N es el límite superior del rango [1, N]
coordenadasImpares n = 
    [ (x, y) 
    | x <- [1..n]     -- Generador 1: x toma valores desde 1 hasta N
    , y <- [1..n]     -- Generador 2: y toma valores desde 1 hasta N
    , (x + y) `mod` 2 /= 0 -- El Filtro mantiene solo si (x + y) es impar
    ]

-- Explicación del Filtro:
-- La condición (x + y) `mod` 2 /= 0 se cumple si:
-- - (x + y) mod 2 es 1 (es decir, la suma es impar).

-------------------------------------------------------------------------------
-- Problema 6: Descomposición Segura de Lista
-------------------------------------------------------------------------------

descomponerListaSegura :: [a] -> Maybe (a, [a])

-- Caso Base 1: Lista vacía.
-- No hay cabeza ni cola, devolvemos Nothing.
descomponerListaSegura [] = Nothing

-- Caso Base 2: Lista no vacía.
-- El patrón (x:xs) descompone la lista en su cabeza (x) y su cola (xs).
-- Devolvemos Just con una tupla (x, xs).
descomponerListaSegura (x:xs) = Just (x, xs)

-------------------------------------------------------------------------------
