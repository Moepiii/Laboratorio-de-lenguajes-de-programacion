module Engine.Parser where

import Data.Char (isSpace, toLower)
import Engine.Types

parseCommand :: String -> Either String Command
parseCommand input = do
  (cmdRaw, rest) <- takeCommandWord input
  let cmd = map toLower cmdRaw
  case cmd of
    "ir" -> Ir <$> parseDirectionFrom rest
    "mirar" -> expectNoArgs "mirar" rest Mirar
    "tomar" -> parseItemArg "tomar" rest Tomar
    "inventario" -> expectNoArgs "inventario" rest Inventario
    "inv" -> expectNoArgs "inv" rest Inv
    "salir" -> expectNoArgs "salir" rest Salir
    "leer" -> parseItemArg "leer" rest Leer
    _ -> Left ("Comando desconocido: '" ++ cmdRaw ++ "'. Usa 'mirar', 'ir <dirección>', 'tomar <objeto>', 'leer <objeto>', 'inventario' o 'salir'.")

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
  "n" -> Just Norte
  "s" -> Just Sur
  "e" -> Just Este
  "o" -> Just Oeste
  _ -> Nothing

takeCommandWord :: String -> Either String (String, String)
takeCommandWord text =
  let trimmed = dropWhile isSpace text
   in if null trimmed
        then Left "Ingresa un comando. Por ejemplo: 'mirar' o 'ir norte'."
        else
          let (command, rest) = break isSpace trimmed
           in Right (command, dropWhile isSpace rest)

parseDirectionFrom :: String -> Either String Direction
parseDirectionFrom rest =
  case dropWhile (\w -> map toLower w `elem` directionPrefixes) (words rest) of
    [] -> Left "Debes indicar una dirección. Ejemplo: 'ir norte' o 'ir al oeste'."
    ws ->
      let dirText = unwords ws
       in maybe (Left ("Dirección desconocida: '" ++ dirText ++ "'.")) Right (parseDirection dirText)
  where
    directionPrefixes = ["a", "al", "hacia", "el", "la", "los", "las"]

parseItemArg :: String -> String -> (String -> Command) -> Either String Command
parseItemArg verb rest constructor =
  let trimmed = trim rest
   in if null trimmed
        then Left ("El comando '" ++ verb ++ "' necesita un objeto. Ejemplo: '" ++ verb ++ " libro'.")
        else Right (constructor trimmed)

expectNoArgs :: String -> String -> Command -> Either String Command
expectNoArgs verb rest command =
  if null (trim rest)
    then Right command
    else Left ("El comando '" ++ verb ++ "' no acepta argumentos.")

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse