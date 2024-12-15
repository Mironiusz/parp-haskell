module Main where

import System.IO
import Control.Monad.State
import GameState
import DataItems
import RunLoop

type Game = StateT GameState IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let st = initialState allNPCs allPositionedItems
  putStrLn "Witaj w grze \"Targówka na Domówku 2: Powrót Andżeja\"!"
  putStrLn "Twoim celem jest zorganizowanie epickiej domówki."
  putStrLn "Wpisz 'help' po pomoc."
  evalStateT gameLoop st
