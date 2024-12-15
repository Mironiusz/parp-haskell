{-# LANGUAGE LambdaCase #-}

module RunLoop (gameLoop) where

import GameState
import Actions
import DataItems
import Dialogues
import PhaseDialogues (phaseDialogue)
import System.IO (hFlush, stdout)
import Control.Monad.State
import Control.Monad (unless, when)
import Data.Char (toLower)

-- | Opisuje aktualną lokalizację gracza oraz wyświetla dostępne ruchy, przedmioty i NPC
describeLocation :: GameState -> IO ()
describeLocation st = do
  putStrLn $ "Znajdujesz się w: " ++ describeLocationName (playerLocation st)
  displayPossibleMoves st
  putStrLn ""
  displayItemsHere st
  putStrLn ""
  displayNpcsHere st

-- | Wyświetla możliwe kierunki ruchu z bieżącej lokalizacji
displayPossibleMoves :: GameState -> IO ()
displayPossibleMoves st = do
  let loc = playerLocation st
      ms = [(d, l2) | (l1, l2, d) <- connections, l1 == loc]
  if null ms
    then putStrLn "Nie ma dostępnych przejść."
    else do
      putStrLn "Możliwe przejścia:"
      mapM_ (\(d, l) -> putStrLn (d ++ " -> " ++ describeLocationName l)) ms

-- | Wyświetla przedmioty dostępne w bieżącej lokalizacji
displayItemsHere :: GameState -> IO ()
displayItemsHere st = do
  let loc = playerLocation st
      itemsHere = [itemName p | p <- positionedItems st, itemLocation p == loc]
  if null itemsHere
    then putStrLn "Nie ma tu nic do podniesienia."
    else do
      putStrLn "Itemy:"
      mapM_ putStrLn itemsHere

-- | Wyświetla NPC obecnych w bieżącej lokalizacji
displayNpcsHere :: GameState -> IO ()
displayNpcsHere st = do
  let loc = playerLocation st
      ns = [npcName n | n <- npcs st, npcLocation n == loc]
  if null ns
    then putStrLn "Nie ma tu nikogo."
    else do
      putStrLn "Osoby:"
      mapM_ putStrLn ns

-- | Funkcja odpowiadająca za ruch gracza w danym kierunku
moveCommand :: String -> GameState -> IO GameState
moveCommand dir st = do
  let loc = playerLocation st
      possible = [(l2, d) | (l1, l2, d) <- connections, l1 == loc, d == dir]
  case possible of
    [] -> do
      putStrLn "Nie możesz tam przejść."
      return st
    ((nextLoc, _):_) -> do
      newSt <- case (playerLocation st, currentPhase st, nextLoc) of
            (PokojMarka, Phase0, PokojBabci) -> do
              let updatedSt = st { playerLocation = PokojBabci, currentPhase = Phase1 }
              phaseDialogue Phase1
              return updatedSt
            (PokojMarka, Phase0, Targowek) -> do
              let updatedSt = st { playerLocation = Blokowa, currentPhase = Phase1 }
              phaseDialogue Phase1
              return updatedSt
            (PokojBabci, Phase1, Makro) -> do
              let updatedSt = st { playerLocation = AlejkaAlkohol, currentPhase = Phase2 }
              phaseDialogue Phase2
              return updatedSt
            (PalarniaSmietnik, Phase2, Wydzial) -> do
              let updatedSt = st { playerLocation = Korytarz1Pietro, currentPhase = Phase3 }
              phaseDialogue Phase3
              return updatedSt
            (Korytarz1Pietro, Phase3, Domowka) -> do
              let guestsCount' = guestsCount st
                  q = fromIntegral (partyQuality st) / fromIntegral guestsCount'
                  gl = guestList st
                  (gl', nps, st') = spawnNpcs q gl st
                  npcRecords = map (\(name, loc) -> NPC name loc) nps
                  updatedSt = st' { playerLocation = Parkiet, currentPhase = Phase4, npcs = npcs st' ++ npcRecords }
              phaseDialogue Phase4
              return updatedSt
            (Parkiet, Phase4, DrzwiWejsciowe) -> do
              let (st2, intro) = initFight st { playerLocation = DrzwiWejsciowe }
              mapM_ putStrLn intro
              phaseDialogue Phase5
              return st2
            _ -> do
              let updatedSt = st { playerLocation = nextLoc }
              return updatedSt
      describeLocation newSt
      return newSt

-- | Główna pętla gry, która obsługuje komendy gracza
gameLoop :: Game ()
gameLoop = do
  st <- get
  liftIO $ putStr "> "
  liftIO $ hFlush stdout
  cmd <- liftIO getLine
  let ws = words cmd
      low = map toLower cmd
  case ws of
    [] -> gameLoop
    ("quit":_) -> do
      liftIO $ putStrLn "Koniec gry."
    ("help":_) -> do
      liftIO $ putStrLn "Komendy: north, east, south, west, describe, pick <przedmiot>, talk <osoba>, use <przedmiot>, help, quit"
      gameLoop
    ("describe":_) -> do
      st' <- get
      liftIO $ describeLocation st'
      gameLoop
    [dir] | dir `elem` ["north", "east", "south", "west"] -> do
      st' <- get
      st2 <- liftIO $ moveCommand dir st'
      put st2
      gameLoop
    ("talk":rest) -> do
      st' <- get
      let npcNameInput = unwords rest
          targetName = normalizeItemName npcNameInput
      if any (\npc -> npcName npc == targetName && npcLocation npc == playerLocation st') (npcs st')
        then do
          talkTo targetName
        else liftIO $ putStrLn "Nie ma tu takiej osoby."
      gameLoop
    ("pick":rest) -> do
      let itemNameInput = unwords rest
          name = normalizeItemName itemNameInput
      st' <- get
      if any (\p -> itemName p == name && itemLocation p == playerLocation st') (positionedItems st')
        then do
          pickUpItem name
        else liftIO $ putStrLn "Nie ma tu takiego przedmiotu."
      gameLoop
    ("use":rest) -> do
      let itemNameInput = unwords rest
          name = normalizeItemName itemNameInput
      useItem name
      st' <- get
      when (inFight st') $ do
        res <- liftIO $ fightCheck st'
        liftIO $ mapM_ putStrLn res
      gameLoop
    _ -> do
      liftIO $ putStrLn "Invalid command."
      gameLoop
