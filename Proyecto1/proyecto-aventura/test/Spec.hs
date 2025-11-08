module Main where

import Data.Either (isLeft, isRight)
import qualified Data.Map as Map
import Engine.Parser
import Engine.Persistence
import Engine.Types
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parseCommand" $ do
    it "acepta comandos de movimiento con preposiciones" $ do
      parseCommand "ir al norte" `shouldBe` Right (Ir Norte)

    it "mantiene nombres de objetos multi-palabra" $ do
      parseCommand "tomar llaves antiguas" `shouldBe` Right (Tomar "llaves antiguas")

    it "rechaza argumentos extra en comandos sin parámetros" $ do
      parseCommand "inventario ahora" `shouldSatisfy` isLeft

    it "normaliza mayúsculas y espacios" $ do
      parseCommand "   IR   SUR  " `shouldBe` Right (Ir Sur)

    it "informa cuando falta un objeto" $ do
      parseCommand "tomar" `shouldBe` Left "El comando 'tomar' necesita un objeto. Ejemplo: 'tomar libro'."

    it "informa cuando el comando es desconocido" $ do
      parseCommand "brincar" `shouldBe` Left "Comando desconocido: 'brincar'. Usa 'mirar', 'ir <dirección>', 'tomar <objeto>', 'leer <objeto>', 'inventario' o 'salir'."

  describe "parseWorldContent" $ do
    it "carga un mundo válido" $ do
      parseWorldContent sampleWorld `shouldSatisfy` isRight
      case parseWorldContent sampleWorld of
        Left err -> expectationFailure ("parseWorldContent falló: " ++ err)
        Right (rooms, items) -> do
          Map.keys rooms `shouldContain` ["Biblioteca"]
          Map.keys items `shouldContain` ["libro"]
          case Map.lookup "Biblioteca" rooms of
            Nothing -> expectationFailure "No se encontró la sala 'Biblioteca'"
            Just room -> roomItems room `shouldBe` ["libro"]

    it "detecta direcciones inválidas" $ do
      parseWorldContent invalidDirectionWorld `shouldSatisfy` isLeft

    it "detecta referencias a objetos inexistentes" $ do
      parseWorldContent missingItemWorld `shouldBe` Left "Sala 'Biblioteca' referencia un objeto inexistente: 'llave'"

    it "detecta salidas hacia salas inexistentes" $ do
      parseWorldContent missingRoomWorld `shouldBe` Left "La sala 'Biblioteca' tiene una salida Norte hacia 'Vestibulo', pero esa sala no existe"

    it "detecta ítems duplicados" $ do
      parseWorldContent duplicateItemWorld `shouldBe` Left "Ítem duplicado: libro"

    it "detecta salas duplicadas" $ do
      parseWorldContent duplicateRoomWorld `shouldBe` Left "Sala duplicada: Biblioteca"

sampleWorld :: String
sampleWorld = unlines
  [ "ITEM: libro"
  , "DESC: Un libro misterioso."
  , "---"
  , "SALA: Biblioteca"
  , "DESC: Una biblioteca silenciosa."
  , "SALIDA: Norte -> Vestibulo"
  , "OBJETO: libro"
  , "---"
  , "SALA: Vestibulo"
  , "DESC: Entrada principal."
  ]

invalidDirectionWorld :: String
invalidDirectionWorld = unlines
  [ "ITEM: llave"
  , "DESC: Una llave dorada."
  , "---"
  , "SALA: Sala"
  , "DESC: Sala sin salidas correctas."
  , "SALIDA: Arriba -> Biblioteca"
  ]

missingItemWorld :: String
missingItemWorld = unlines
  [ "ITEM: libro"
  , "DESC: Un libro misterioso."
  , "---"
  , "SALA: Biblioteca"
  , "DESC: Sala con referencias inválidas."
  , "OBJETO: llave"
  ]

missingRoomWorld :: String
missingRoomWorld = unlines
  [ "ITEM: libro"
  , "DESC: Un libro misterioso."
  , "---"
  , "SALA: Biblioteca"
  , "DESC: Sala con salida fantasma."
  , "SALIDA: Norte -> Vestibulo"
  , "OBJETO: libro"
  ]

duplicateItemWorld :: String
duplicateItemWorld = unlines
  [ "ITEM: libro"
  , "DESC: Un libro misterioso."
  , "---"
  , "ITEM: libro"
  , "DESC: Otra descripción."
  , "---"
  , "SALA: Biblioteca"
  , "DESC: Sala con duplicados."
  , "OBJETO: libro"
  ]

duplicateRoomWorld :: String
duplicateRoomWorld = unlines
  [ "ITEM: libro"
  , "DESC: Un libro misterioso."
  , "---"
  , "SALA: Biblioteca"
  , "DESC: Sala original."
  , "OBJETO: libro"
  , "---"
  , "SALA: Biblioteca"
  , "DESC: Sala duplicada."
  ]