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
  putStrLn $ "Znajdujesz się w: " ++ describeLocationName (playerLocation st) ++ "."
  displayPossibleMoves st
  putStrLn ""
  displayItemsHere st
  putStrLn ""
  displayNpcsHere st

-- | Opisuje aktualny ekwipunek gracza
describeInventory :: GameState -> IO ()
describeInventory st = do
  displayInventory st

-- | Opisuje aktualny ekwipunek gracza
describePartyInventory :: GameState -> IO ()
describePartyInventory st = do
  putStrLn $ "Masz " ++ show (playerMoney st) ++ " zł."
  displayPartyInventory st

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
      mapM_ (putStrLn . ("  -> " ++)) itemsHere

-- | Wyświetla przedmioty w ekwipunku gracza
displayInventory :: GameState -> IO ()
displayInventory st = do
  let inventory = playerInventory st
  if null inventory
    then putStrLn "Twój ekwipunek jest pusty."
    else do
      putStrLn "Twój ekwipunek zawiera:"
      mapM_ (putStrLn . ("  -> " ++)) inventory

-- | Wyświetla przedmioty w ekwipunku gracza
displayPartyInventory :: GameState -> IO ()
displayPartyInventory st = do
  let inventory = partyInventory st
  if null inventory
    then putStrLn "Twój ekwipunek jest pusty."
    else do
      putStrLn "Na imprezę kupiłeś:"
      mapM_ (putStrLn . ("  -> " ++)) inventory

-- | Wyświetla NPC obecnych w bieżącej lokalizacji
displayNpcsHere :: GameState -> IO ()
displayNpcsHere st = do
  let loc = playerLocation st
      ns = [npcName n | n <- npcs st, npcLocation n == loc]
  if null ns
    then putStrLn "Nie ma tu nikogo."
    else do
      putStrLn "Osoby:"
      mapM_ (putStrLn . ("  -> " ++)) ns

displayHelp :: IO ()
displayHelp = do
  putStrLn "=== Lista dostępnych komend ==="
  putStrLn "north, east, south, west"
  putStrLn "  -> Przemieszczasz się w wybranym kierunku, jeśli jest to możliwe."
  putStrLn "describe"
  putStrLn "  -> Wyświetla opis aktualnej lokalizacji, w tym przedmioty i NPC w pobliżu."
  putStrLn "inventory"
  putStrLn "  -> Wyświetla listę przedmiotów w twoim ekwipunku."
  putStrLn "party_inventory"
  putStrLn "  -> Wyświetla listę przedmiotów zgromadzonych na imprezę. Tutaj są też pieniądze."
  putStrLn "pick <przedmiot>"
  putStrLn "  -> Podnosi przedmiot znajdujący się w bieżącej lokalizacji."
  putStrLn "talk <osoba>"
  putStrLn "  -> Rozpoczyna rozmowę z wybraną postacią (NPC) w bieżącej lokalizacji."
  putStrLn "use <przedmiot>"
  putStrLn "  -> Używa wybranego przedmiotu z twojego ekwipunku (np. podczas walki)."
  putStrLn "help"
  putStrLn "  -> Wyświetla tę pomoc."
  putStrLn "s_help"
  putStrLn "  -> Wyświetla komendy, bez objaśnień."
  putStrLn "quit"
  putStrLn "  -> Wyjście z gry."
  putStrLn "================================"

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
            (Ogrodnicza, Phase1, Makro) -> do
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
                  npcRecords = [NPC n l | (n,l)<-nps]
                  updatedSt = st' {playerLocation = Parkiet, currentPhase = Phase4, npcs = npcs st' ++ npcRecords}
              phaseDialogue Phase4
              return updatedSt
            (Parkiet, Phase4, DrzwiWejsciowe) -> do
              let (st2, intro) = initFight st { playerLocation = DrzwiWejsciowe }
              mapM_ putStrLn intro
              phaseDialogue Phase5
              describeInventory st2
              return st2
            _ -> do
              let updatedSt = st { playerLocation = nextLoc }
              return updatedSt

      unless (inFight newSt) $ describeLocation newSt
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
      liftIO displayHelp
      gameLoop
    ("s_help":_) -> do
      liftIO $ putStrLn "Komendy: north, east, south, west, describe, inventory, party_inventory, pick <przedmiot>, talk <osoba>, use <przedmiot>, help, s_help, quit"
      gameLoop
    ("describe":_) -> do
      st' <- get
      liftIO $ describeLocation st'
      gameLoop
    ("inventory":_) -> do
      st' <- get
      liftIO $ describeInventory st'
      gameLoop
    ("party_inventory":_) -> do
      st' <- get
      liftIO $ describePartyInventory st'
      gameLoop
    [dir] | dir `elem` ["north", "east", "south", "west"] -> do  -- Poprawka tutaj
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
        liftIO $ fightCheck st'
      gameLoop
    _ -> do
      liftIO $ putStrLn "Invalid command."
      gameLoop