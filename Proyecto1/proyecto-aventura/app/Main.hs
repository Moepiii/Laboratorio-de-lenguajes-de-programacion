{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Map qualified as Map
import Engine.Core
import Engine.Parser
import Engine.Persistence
import Engine.Types
import System.IO

main :: IO ()
main = do
  putStrLn "=== Motor de Aventura de Texto ==="

  -- Cargar el mundo
  worldResult <- loadWorldData "mundo.txt"

  case worldResult of
    Left errorMsg -> do
      putStrLn $ "Error cargando el mundo: " ++ errorMsg
      putStrLn "Asegúrate de que el archivo 'mundo.txt' existe y tiene el formato correcto."
    Right (rooms, items) -> do
      putStrLn "Mundo cargado exitosamente!"

      case Map.lookupMin rooms of
        Nothing -> putStrLn "Error: el archivo 'mundo.txt' no define salas."
        Just (startRoom, startRoomData) -> do
          let initialState =
                GameState
                  { currentRoom = startRoom,
                    playerInventory = [],
                    worldRooms = rooms,
                    worldItems = items
                  }

          putStrLn $ "\n" ++ roomDesc startRoomData
          gameLoop initialState

gameLoop :: GameState -> IO ()
gameLoop state = do
  putStr "\n> "
  hFlush stdout
  input <- getLine

  case parseCommand input of
    Left errMsg -> do
      putStrLn errMsg
      gameLoop state
    Right command -> do
      let (message, newState) = processCommand command state

      putStrLn message

      case command of
        Salir -> return ()
        _ -> gameLoop newState