{-# LANGUAGE ImportQualifiedPost #-}

module Engine.Core where

import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Engine.Types

processCommand :: Command -> GameState -> (String, GameState)
processCommand cmd state = case cmd of
  Ir dir -> processMove dir state
  Mirar -> processLook state
  Tomar obj -> processTake obj state
  Inventario -> processInventory state
  Inv -> processInventory state
  Salir -> ("¡Hasta la próxima!", state)
  Leer obj -> processRead obj state -- NUEVO: procesar comando leer

processMove :: Direction -> GameState -> (String, GameState)
processMove dir state =
  case Map.lookup (currentRoom state) (worldRooms state) of
    Nothing -> ("Error: Sala actual no encontrada", state)
    Just currentRoomData ->
      case Map.lookup dir (roomExits currentRoomData) of
        Nothing -> ("No puedes ir en esa dirección", state)
        Just nextRoom ->
          case Map.lookup nextRoom (worldRooms state) of
            Nothing -> ("Error: Sala destino no encontrada", state)
            Just nextRoomData ->
              ( "Te mueves hacia " ++ show dir ++ "\n" ++ roomDesc nextRoomData,
                state {currentRoom = nextRoom}
              )

processLook :: GameState -> (String, GameState)
processLook state =
  case Map.lookup (currentRoom state) (worldRooms state) of
    Nothing -> ("Error: Sala actual no encontrada", state)
    Just room ->
      let desc = roomDesc room
          items =
            if null (roomItems room)
              then ""
              else "\nObjetos aquí: " ++ intercalate ", " (roomItems room)
          exits = "\nSalidas: " ++ intercalate ", " (map show (Map.keys (roomExits room)))
       in (desc ++ items ++ exits, state)

processTake :: String -> GameState -> (String, GameState)
processTake itemName state =
  case Map.lookup (currentRoom state) (worldRooms state) of
    Nothing -> ("Error: Sala actual no encontrada", state)
    Just room ->
      if itemName `elem` roomItems room
        then
          let updatedRoom = room {roomItems = filter (/= itemName) (roomItems room)}
              updatedRooms = Map.insert (currentRoom state) updatedRoom (worldRooms state)
              updatedInventory = itemName : playerInventory state
           in ( "Tomas: " ++ itemName,
                state {playerInventory = updatedInventory, worldRooms = updatedRooms}
              )
        else
          ("No hay '" ++ itemName ++ "' aquí", state)

processInventory :: GameState -> (String, GameState)
processInventory state =
  if null (playerInventory state)
    then ("Tu inventario está vacío", state)
    else ("Inventario: " ++ intercalate ", " (playerInventory state), state)

-- NUEVO: Procesar comando leer
processRead :: String -> GameState -> (String, GameState)
processRead itemName state =
  if itemName `elem` playerInventory state
    then case Map.lookup itemName (worldItems state) of
      Just item -> ("Lees '" ++ itemName ++ "': " ++ itemDesc item, state)
      Nothing -> ("No puedes leer '" ++ itemName ++ "'", state)
    else
      ("No tienes '" ++ itemName ++ "' en tu inventario", state)