{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Monad
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

      -- Encontrar sala inicial (primera sala en el mapa)
      let startRoom = case Map.keys rooms of
            [] -> error "No hay salas en el mundo"
            (firstRoom : _) -> firstRoom

      -- Estado inicial
      let initialState =
            GameState
              { currentRoom = startRoom,
                playerInventory = [],
                worldRooms = rooms,
                worldItems = items
              }

      -- Mostrar sala inicial
      case Map.lookup startRoom rooms of
        Just room -> putStrLn $ "\n" ++ roomDesc room
        Nothing -> putStrLn "Error: Sala inicial no encontrada"

      -- Iniciar bucle del juego
      gameLoop initialState

gameLoop :: GameState -> IO ()
gameLoop state = do
  putStr "\n> "
  hFlush stdout
  input <- getLine

  if null input
    then gameLoop state
    else do
      let cmd = parseCommand input

      case cmd of
        Nothing -> do
          putStrLn "Comando no válido. Comandos disponibles: ir <dirección>, mirar, tomar <objeto>, coger <objeto>, leer <objeto>, inventario, salir"
          gameLoop state
        Just command -> do
          let (message, newState) = processCommand command state

          putStrLn message

          case command of
            Salir -> return ()
            _ -> gameLoop newState