{-# LANGUAGE ImportQualifiedPost #-}

module Engine.Types where

import Data.Map (Map)
import Data.Map qualified as Map

-- Direcciones
data Direction = Norte | Sur | Este | Oeste | Noroeste | Noreste | Suroeste | Sureste
  deriving (Show, Eq, Ord, Enum, Bounded)

-- Comandos
data Command
  = Ir Direction
  | Mirar
  | Tomar String
  | Coger String
  | Inventario
  | Inv
  | Salir
  | Leer String -- NUEVO: leer objetos
  | ComandoInvalido String
  deriving (Show, Eq)

-- Objetos
data Item = Item
  { itemName :: String,
    itemDesc :: String
  }  deriving (Show, Eq)

-- Salas
data Room = Room
  { roomName :: String,
    roomDesc :: String,
    roomExits :: Map Direction String,
    roomItems :: [String]
  }
  deriving (Show, Eq)

-- Estado del juego
data GameState = GameState
  { currentRoom :: String,
    playerInventory :: [String],
    worldRooms :: Map String Room,
    worldItems :: Map String Item
  }
  deriving (Show, Eq)

-- Tipos para nombres
type RoomName = String

type ItemName = String

-- Estado inicial
initialState :: Map String Room -> Map String Item -> String -> GameState
initialState rooms items startRoom =
  GameState
    { currentRoom = startRoom,
      playerInventory = [],
      worldRooms = rooms,
      worldItems = items
    }
