module Main where

import Tarea1 -- Importa el módulo de tareas

-- Función principal que inicia el menú
main :: IO ()
main = menu
-- Función principal que inicia el menú
menu :: IO ()
menu = do
  putStrLn "\n--- TAREA DE HASKELL ---"
  putStrLn "Selecciona el programa a ejecutar (Ejemplos Fijos):"
  putStrLn "1. Probar esPalindromo (Problema 1)"
  putStrLn "2. Probar productoParesRec (Problema 2)"
  putStrLn "3. Probar parsearCondicional (Problema 3)"
  putStrLn "4. Probar sumaAcumuladaCondicional (Problema 4)"
  putStrLn "5. Probar coordenadasImpares (Problema 5)"
  putStrLn "6. Probar descomponerListaSegura (Problema 6)" 
  putStrLn "7. Salir" 
  putStr "Opción: "

  opcionStr <- getLine

  case opcionStr of
    "1" -> do
      pruebaPalindromoFija
      menu
    "2" -> do
      pruebaProductoParesFija
      menu
    "3" -> do
      pruebaParsearCondicionalFija
      menu
    "4" -> do
      pruebaSumaCondicionalFija
      menu
    "5" -> do
      pruebaCoordenadasImparesFija
      menu
    "6" -> do
      pruebaDescomponerListaFija
      menu
    "7" -> putStrLn "¡Hasta luego! Programa finalizado."
    _ -> do
      putStrLn "Opción no válida. Inténtalo de nuevo."
      menu

-- PRUEBA PARA PROBLEMA 1: esPalindromo
pruebaPalindromoFija :: IO ()
pruebaPalindromoFija = do
    putStrLn "\n--- Prueba Fija: esPalindromo ---"
    
    let pal1 = "oso"
    let pal2 = "haslo"
    let pal3 = "reconocer"
    let pal4 = ""
    
    putStrLn $ "Es \"" ++ pal1 ++ "\" un palíndromo? " ++ show (esPalindromo pal1)
    putStrLn $ "Es \"" ++ pal2 ++ "\" un palíndromo? " ++ show (esPalindromo pal2)
    putStrLn $ "Es \"" ++ pal3 ++ "\" un palíndromo? " ++ show (esPalindromo pal3)
    putStrLn $ "Es \"\" un palíndromo? " ++ show (esPalindromo pal4)

-- PRUEBA PARA PROBLEMA 2: productoParesRec
pruebaProductoParesFija :: IO ()
pruebaProductoParesFija = do
    putStrLn "\n--- Prueba Fija: productoParesRec ---"
    
    let lista1 = [1, 2, 3, 4, 5, 6]
    let lista2 = [1, 3, 5, 7]
    let lista3 = [2, 4, 6]
    
    let lista4 :: [Integer]
        lista4 = [] 
    
    putStrLn $ "Lista: " ++ show lista1 ++ " -> Producto de pares: " ++ show (productoParesRec lista1)
    putStrLn $ "Lista: " ++ show lista2 ++ " -> Producto de pares: " ++ show (productoParesRec lista2) 
    putStrLn $ "Lista: " ++ show lista3 ++ " -> Producto de pares: " ++ show (productoParesRec lista3)
    putStrLn $ "Lista: " ++ show lista4 ++ " -> Producto de pares: " ++ show (productoParesRec lista4)

-- PRUEBA PARA PROBLEMA 3: parsearCondicional
pruebaParsearCondicionalFija :: IO ()
pruebaParsearCondicionalFija = do
    putStrLn "\n--- Prueba Fija: parsearCondicional ---"
    
    let listaCadenas = ["123", "Haskell", "45.6", "0", "CI3661", "-99"]
    let resultado = parsearCondicional listaCadenas
    
    putStrLn $ "Entrada: " ++ show listaCadenas
    putStrLn $ "Resultado: " ++ show resultado
    
    putStrLn "\nResultados esperados:"
    putStrLn " - \"123\" -> Right 123"
    putStrLn " - \"Haskell\" -> Left \"HASKELL\""
    putStrLn " - \"-99\" -> Right -99"

    -- PRUEBA PARA PROBLEMA 4: sumaAcumuladaCondicional
pruebaSumaCondicionalFija :: IO ()
pruebaSumaCondicionalFija = do
    putStrLn "\n--- Prueba Fija: sumaAcumuladaCondicional ---"
    
    let umbral1 = 5.0
    let lista1  = [1.5, 6.0, 4.0, 7.5, 3.2]
    
    let umbral2 = 10.0
    let lista2  = [1.0, 20.0, 5.0, 15.0]
    
    let resultado1 = sumaAcumuladaCondicional umbral1 lista1
    let resultado2 = sumaAcumuladaCondicional umbral2 lista2

    putStrLn $ "Umbral: " ++ show umbral1 ++ ", Lista: " ++ show lista1
    putStrLn $ " -> Suma (mayores a 5.0): " ++ show resultado1 

    putStrLn $ "Umbral: " ++ show umbral2 ++ ", Lista: " ++ show lista2
    putStrLn $ " -> Suma (mayores a 10.0): " ++ show resultado2 

    putStrLn $ "Lista vacía con umbral 0.0: " ++ show (sumaAcumuladaCondicional 0.0 [])

    -- PRUEBA PARA PROBLEMA 5: coordenadasImpares
pruebaCoordenadasImparesFija :: IO ()
pruebaCoordenadasImparesFija = do
    putStrLn "\n--- Prueba Fija: coordenadasImpares ---"
    
    let n1 = 2
    let n2 = 3
    
    let resultado1 = coordenadasImpares n1
    let resultado2 = coordenadasImpares n2
    
    putStrLn $ "Para N=" ++ show n1 ++ " (Rango [1, 2]): "
    putStrLn $ " -> Resultado: " ++ show resultado1

    putStrLn $ "Para N=" ++ show n2 ++ " (Rango [1, 3]): "
    putStrLn $ " -> Resultado: " ++ show resultado2

    -- PRUEBA PARA PROBLEMA 6: descomponerListaSegura
pruebaDescomponerListaFija :: IO ()
pruebaDescomponerListaFija = do
    putStrLn "\n--- Prueba Fija: descomponerListaSegura ---"
    
    let lista1 = [1, 2, 3] :: [Int]
    let lista2 = ["a"]     :: [String]
    let lista3 = []        :: [Int]
    
    let resultado1 = descomponerListaSegura lista1
    let resultado2 = descomponerListaSegura lista2
    let resultado3 = descomponerListaSegura lista3
    
    putStrLn $ "Lista: " ++ show lista1
    putStrLn $ " -> Resultado: " ++ show resultado1
    
    putStrLn $ "Lista: " ++ show lista2
    putStrLn $ " -> Resultado: " ++ show resultado2

    putStrLn $ "Lista: " ++ show lista3
    putStrLn $ " -> Resultado: " ++ show resultado3
