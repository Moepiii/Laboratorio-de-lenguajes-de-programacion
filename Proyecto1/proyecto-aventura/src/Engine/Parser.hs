module Engine.Parser where

import Data.Char (toLower)
import Engine.Types

parseCommand :: String -> Maybe Command
parseCommand input = case words (map toLower input) of
  ["ir", dir] -> Ir <$> parseDirection dir
  ["mirar"] -> Just Mirar
  ["tomar", obj] -> Just (Tomar obj)
  ["coger", obj] -> Just (Coger obj)
  ["inventario"] -> Just Inventario
  ["inv"] -> Just Inv
  ["salir"] -> Just Salir
  ["leer", obj] -> Just (Leer obj) -- NUEVO: parsear comando leer
  _ -> Nothing

parseDirection :: String -> Maybe Direction
parseDirection dir = case map toLower dir of
  "norte" -> Just Norte
  "sur" -> Just Sur
  "este" -> Just Este
  "oeste" -> Just Oeste
  "noroeste" -> Just Noroeste
  "noreste" -> Just Noreste
  "suroeste" -> Just Suroeste
  "sureste" -> Just Sureste
  _ -> Nothing

-- Función auxiliar para parsear el archivo de mundo
parseWorldLine :: String -> (String, String)
parseWorldLine line =
  let (key, value) = break (== ':') line
   in (trim key, trim (drop 1 value))
  where
    trim = unwords . words