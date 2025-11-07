{-# LANGUAGE ImportQualifiedPost #-}

module Engine.Persistence where

import Control.Exception
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Engine.Types
import System.IO

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
parseWorldContent content =
  let blocks = splitBlocks content
      (itemBlocks, roomBlocks) = partitionBlocks blocks
      items = parseItems itemBlocks
      rooms = parseRooms roomBlocks items
   in Right (Map.fromList rooms, Map.fromList items)

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

partitionBlocks :: [String] -> ([String], [String])
partitionBlocks = foldr classify ([], [])
  where
    classify block (items, rooms) =
      let firstLine = head (lines block)
       in if "ITEM:" `isPrefixOf` firstLine
            then (block : items, rooms)
            else (items, block : rooms)

parseItems :: [String] -> [(String, Item)]
parseItems = mapMaybe parseItemBlock
  where
    parseItemBlock block =
      let lines' = lines block
          nameLine = findLine "ITEM:" lines'
          descLine = findLine "DESC:" lines'
       in case (nameLine, descLine) of
            (Just name, Just desc) ->
              let cleanName = cleanString name
               in Just (cleanName, Item cleanName desc)
            _ -> Nothing

    findLine prefix lines' =
      case filter (isPrefixOf prefix) lines' of
        [] -> Nothing
        (x : _) -> Just (drop (length prefix) x)

parseRooms :: [String] -> [(String, Item)] -> [(String, Room)]
parseRooms blocks items = mapMaybe (parseRoomBlock items) blocks

parseRoomBlock :: [(String, Item)] -> String -> Maybe (String, Room)
parseRoomBlock items block =
  let lines' = lines block
      nameLine = findLine "SALA:" lines'
      descLine = findLine "DESC:" lines'
      exitLines = filter (isPrefixOf "SALIDA:") lines'
      itemLines = filter (isPrefixOf "OBJETO:") lines'
   in case (nameLine, descLine) of
        (Just name, Just desc) ->
          let cleanName = cleanString name
              exits = map parseExit exitLines
              roomItems = map (cleanString . drop (length "OBJETO:")) itemLines
              validItems = filter (`elem` map fst items) roomItems
           in Just (cleanName, Room cleanName desc (Map.fromList exits) validItems)
        _ -> Nothing
  where
    findLine prefix lines' =
      case filter (isPrefixOf prefix) lines' of
        [] -> Nothing
        (x : _) -> Just (drop (length prefix) x)

    parseExit line =
      let content = drop (length "SALIDA:") line
          (dirStr, roomStr) = break (== '-') content
          direction = case parseDirection (cleanString dirStr) of
            Just dir -> dir
            Nothing -> Norte -- Default
          roomName = cleanString $ drop 2 roomStr -- Skip "->"
       in (direction, roomName)

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
    parseDirection' _ = Nothing

-- Función auxiliar para limpiar strings
cleanString :: String -> String
cleanString = unwords . words . filter (`notElem` "\r")

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> case f x of Just y -> y : acc; Nothing -> acc) []

findM :: (a -> Bool) -> [a] -> Maybe a
findM _ [] = Nothing
findM p (x : xs) = if p x then Just x else findM p xs