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
  putStrLn "Nazywasz się Marek i jesteś studentem informatyki na WEiTI. Chcesz zorganizować domówkę, ale Cię na to nie stać."
  putStrLn "Postanawiasz stawić czoła wszystkim przeciwnościom losu i zarobić pieniądze na organizację imprezy."
  putStrLn "Przychodzą Ci do głowy dwie opcje: pozyskać pieniądze sprzedając tajeminiczy proszek do prania na Targówku albo \"pożyczając\" je od babci."
  putStrLn "Jak grać:"
  putStrLn "- Warto zbierać wszystkie przedmioty, które znajdziesz"
  putStrLn "- Warto rozmawiać z każdym napotkanym NPC"
  putStrLn "- Warto dokładnie zwiedzić każdą lokację"
  putStrLn "- Uważaj na zagrożenia czyhające na Targówku"
  putStrLn "- Zarób pieniądze i zorganizuj domówkę!"
  putStrLn "Wpisz 'help' po pomoc w sterowaniu."
  evalStateT gameLoop st