-------------------------------------------------------------------------------
-- Tests.hs - Pruebas Unitarias para Tarea 1
-- Usa el framework HUnit. Para compilar y correr:
-- 1. Asegúrate de tener la librería HUnit instalada (e.g., usando cabal o stack).
-- 2. Compila: ghc --make Tests.hs
-- 3. Ejecuta: ./Tests
-------------------------------------------------------------------------------

import Tarea1 
import Test.HUnit 
import Data.Char (toUpper) -- Necesario para Problema 3 si la implementación usa toUpper

-------------------------------------------------------------------------------
-- 1. Pruebas para esPalindromo
-------------------------------------------------------------------------------

testPalindromo :: Test
testPalindromo = "Palíndromo simple" ~: esPalindromo "oso" ~?= True

-------------------------------------------------------------------------------
-- 2. Pruebas para productoParesRec
-------------------------------------------------------------------------------

testProductoParesRec :: Test
testProductoParesRec = "Solo pares" ~: productoParesRec [2, 4, 6] ~?= 48

-------------------------------------------------------------------------------
-- 3. Pruebas para parsearCondicional
-------------------------------------------------------------------------------

-- Nota: Para que estas pruebas funcionen correctamente, la implementación
-- del alumno debe usar funciones estándar de Haskell para toUpper y para
-- intentar parsear a Int (e.g., readMaybe).

testParsearCondicional :: Test
testParsearCondicional = "Solo números válidos" ~: parsearCondicional ["42"] ~?= [Right 42]

-------------------------------------------------------------------------------
-- 4. Pruebas para sumaAcumuladaCondicional
-------------------------------------------------------------------------------

testSumaCondicional :: Test
testSumaCondicional = "Umbral 5.0, todos mayores" ~: sumaAcumuladaCondicional 5.0 [6.0, 7.0, 8.0] ~?= 21.0

-------------------------------------------------------------------------------
-- 5. Pruebas para coordenadasImpares
-------------------------------------------------------------------------------

testCoordenadasImpares :: Test
testCoordenadasImpares = "N=2" ~: coordenadasImpares 2 ~?= [(1, 2), (2, 1)]

-------------------------------------------------------------------------------
-- 6. Pruebas para descomponerListaSegura
-------------------------------------------------------------------------------

testDescomponerListaSegura :: Test
testDescomponerListaSegura = "Un elemento" ~: descomponerListaSegura [1] ~?= Just (1, [])

-------------------------------------------------------------------------------
-- Ejecución Principal
-------------------------------------------------------------------------------

-- Lista principal de todos los tests
allTests :: Test
allTests = TestList
    [ TestLabel "Problema 1: esPalindromo" testPalindromo
    , TestLabel "Problema 2: productoParesRec" testProductoParesRec
    , TestLabel "Problema 3: parsearCondicional" testParsearCondicional
    , TestLabel "Problema 4: sumaAcumuladaCondicional" testSumaCondicional
    , TestLabel "Problema 5: coordenadasImpares" testCoordenadasImpares
    , TestLabel "Problema 6: descomponerListaSegura" testDescomponerListaSegura
    ]

main :: IO Counts
main = runTestTT allTests
