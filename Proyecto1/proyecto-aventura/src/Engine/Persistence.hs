{-# LANGUAGE ImportQualifiedPost #-}

module Engine.Persistence where

import Control.Exception
import Control.Monad (foldM, unless, when)
import Data.Char (isSpace, toLower)
import Data.List (find, isPrefixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Engine.Types

loadWorldData :: FilePath -> IO (Either String (Map String Room, Map String Item))
loadWorldData filePath =
  catch (loadWorldData' filePath) handleIOError
  where
    handleIOError :: IOError -> IO (Either String (Map String Room, Map String Item))
    handleIOError e = return $ Left $ "Error de E/S: " ++ show e

loadWorldData' :: FilePath -> IO (Either String (Map String Room, Map String Item))
loadWorldData' filePath = do
  content <- readFile filePath
  return $ parseWorldContent content

parseWorldContent :: String -> Either String (Map String Room, Map String Item)
parseWorldContent content = do
  let blocks = splitBlocks content
  unless (not (null blocks)) (Left "El archivo 'mundo.txt' está vacío")
  (itemBlocks, roomBlocks) <- partitionBlocks blocks
  items <- parseItems itemBlocks
  rooms <- parseRooms roomBlocks items
  unless (not (Map.null rooms)) (Left "El archivo 'mundo.txt' no define salas")
  validateRoomTargets rooms
  pure (rooms, items)

splitBlocks :: String -> [String]
splitBlocks = filter (not . null) . map unlines . splitOn "---" . lines
  where
    splitOn :: (Eq a) => [a] -> [[a]] -> [[[a]]]
    splitOn _ [] = []
    splitOn delim list =
      let (chunk, rest) = break (== delim) list
       in chunk : case rest of
            [] -> []
            _ : xs -> splitOn delim xs

partitionBlocks :: [String] -> Either String ([String], [String])
partitionBlocks = foldM classify ([], [])
  where
    classify (items, rooms) block =
      case lines block of
        [] -> Left "Se encontró un bloque vacío en 'mundo.txt'"
        firstLine : _ ->
          let trimmed = dropWhile isSpace firstLine
           in if "ITEM:" `isPrefixOf` trimmed
                then Right (block : items, rooms)
                else
                  if "SALA:" `isPrefixOf` trimmed
                    then Right (items, block : rooms)
                    else Left ("Formato desconocido de bloque: " ++ trimmed)

parseItems :: [String] -> Either String (Map String Item)
parseItems = foldM step Map.empty
  where
    step acc block = do
      let ls = lines block
      rawName <- lookupLine "ITEM:" ls
      rawDesc <- lookupLine "DESC:" ls
      let name = cleanString rawName
      when (null name) (Left "Se encontró un ITEM sin nombre")
      when (Map.member name acc) (Left ("Ítem duplicado: " ++ name))
      let desc = cleanString rawDesc
      pure (Map.insert name (Item name desc) acc)

parseRooms :: [String] -> Map String Item -> Either String (Map String Room)
parseRooms blocks items = foldM step Map.empty blocks
  where
    step acc block = do
      (name, room) <- parseRoomBlock items block
      when (Map.member name acc) (Left ("Sala duplicada: " ++ name))
      pure (Map.insert name room acc)

parseRoomBlock :: Map String Item -> String -> Either String (String, Room)
parseRoomBlock items block = do
  let ls = lines block
  rawName <- lookupLine "SALA:" ls
  rawDesc <- lookupLine "DESC:" ls
  let roomName = cleanString rawName
  when (null roomName) (Left "Se encontró una sala sin nombre")
  exits <- foldM (accumulateExit roomName) Map.empty (filter (isPrefixOf "SALIDA:") ls)
  roomItems <- fmap reverse $ foldM (accumulateItem roomName) [] (filter (isPrefixOf "OBJETO:") ls)
  let desc = cleanString rawDesc
  pure (roomName, Room roomName desc exits roomItems)
  where
    accumulateExit roomName acc line = do
      (dir, destination) <- parseExit roomName line
      when (Map.member dir acc) (Left ("Sala '" ++ roomName ++ "' define dos salidas hacia " ++ show dir))
      pure (Map.insert dir destination acc)

    accumulateItem roomName acc line = do
      let itemName = cleanString (drop (length "OBJETO:") line)
      when (null itemName) (Left ("Sala '" ++ roomName ++ "' referencia un objeto sin nombre"))
      when (itemName `elem` acc) (Left ("Sala '" ++ roomName ++ "' lista el objeto '" ++ itemName ++ "' más de una vez"))
      case Map.lookup itemName items of
        Nothing -> Left ("Sala '" ++ roomName ++ "' referencia un objeto inexistente: '" ++ itemName ++ "'")
        Just _ -> pure (itemName : acc)

parseExit :: String -> String -> Either String (Direction, String)
parseExit roomName line =
  let content = drop (length "SALIDA:") line
      (dirStr, rest) = break (== '-') content
      directionName = cleanString dirStr
   in do
        when (null directionName) (Left ("Sala '" ++ roomName ++ "' define una salida sin dirección"))
        direction <- maybe (Left ("Dirección inválida '" ++ directionName ++ "' en sala '" ++ roomName ++ "'")) Right (parseDirection directionName)
        case rest of
          ('-' : '>' : dest) ->
            let destination = cleanString dest
             in if null destination
                  then Left ("Sala '" ++ roomName ++ "' define una salida sin sala destino")
                  else Right (direction, destination)
          _ -> Left ("Formato de salida inválido en sala '" ++ roomName ++ "'. Usa 'SALIDA: Direccion -> Sala'")

parseDirection :: String -> Maybe Direction
parseDirection = parseDirection' . map toLower . cleanString
  where
    parseDirection' "norte" = Just Norte
    parseDirection' "sur" = Just Sur
    parseDirection' "este" = Just Este
    parseDirection' "oeste" = Just Oeste
    parseDirection' "noroeste" = Just Noroeste
    parseDirection' "noreste" = Just Noreste
    parseDirection' "suroeste" = Just Suroeste
    parseDirection' "sureste" = Just Sureste
    parseDirection' "n" = Just Norte
    parseDirection' "s" = Just Sur
    parseDirection' "e" = Just Este
    parseDirection' "o" = Just Oeste
    parseDirection' _ = Nothing

lookupLine :: String -> [String] -> Either String String
lookupLine prefix ls =
  case find (prefix `isPrefixOf`) ls of
    Just line -> Right (drop (length prefix) line)
    Nothing -> Left ("No se encontró la línea '" ++ prefix ++ "' en un bloque de 'mundo.txt'")

validateRoomTargets :: Map String Room -> Either String ()
validateRoomTargets rooms =
  case firstMissing of
    Nothing -> Right ()
    Just (roomName, dir, target) -> Left ("La sala '" ++ roomName ++ "' tiene una salida " ++ show dir ++ " hacia '" ++ target ++ "', pero esa sala no existe")
  where
    firstMissing =
      find
        (\(_, _, target) -> Map.notMember target rooms)
        [ (roomName, dir, target)
        | (roomName, room) <- Map.toList rooms,
          (dir, target) <- Map.toList (roomExits room)
        ]

-- Función auxiliar para limpiar strings
cleanString :: String -> String
cleanString = unwords . words . filter (`notElem` "\r")