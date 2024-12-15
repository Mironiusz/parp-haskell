{-# LANGUAGE LambdaCase #-}

module Actions where

import GameState
import DataItems
import Dialogues
import PhaseDialogues (phaseDialogue)
import Data.Char (toLower)
import Data.List (find, delete)
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless, when)

type Game = StateT GameState IO

normalizeItemName :: String -> String
normalizeItemName = map (\c -> if c == '\'' then '_' else toLower c)

checkIfTooGreedy :: Int -> Game ()
checkIfTooGreedy c = do
  st <- get
  if c == 4
    then do
      let updatedSt = st {
            playerLocation = AlejkaAlkohol,
            currentPhase = Phase2,
            moneyItemTaken = 5
          }
      put updatedSt
      liftIO $ phaseDialogue Phase2
    else return ()

addItemInternal :: String -> GameState -> (GameState, String)
addItemInternal item st =
  case find (\m -> mName m == item) allMoneyItems of
    Just mi ->
      let val = mValue mi
          newMoney = playerMoney st + val
          count = moneyItemTaken st + 1
          st2 = st { playerMoney = newMoney, moneyItemTaken = count }
          msg = "Otrzymałeś " ++ show val ++ " zł."
      in (checkIfTooGreedyPure count st2, msg)

    Nothing ->
      case find (\p -> pName p == item) allPartyItems of
        Just pi ->
          let price = pPrice pi
              q = pQuality pi
              cm = playerMoney st
          in if cm - price < 0
             then (st, "Przedmiot jest za drogi\nZostało Ci " ++ show cm ++ " zł.")
             else
               let newMoney = cm - price
                   newPartyInv = item : partyInventory st
                   newQual = partyQuality st + q
                   st2 = st { playerMoney = newMoney, partyInventory = newPartyInv, partyQuality = newQual }
                   msg = "Wydałeś " ++ show price ++ " zł.\nZostało Ci " ++ show newMoney ++ " zł."
               in (st2, msg)
        Nothing ->
          case find (\f -> fName f == item) allFightItems of
            Just _ ->
              let newInv = item : playerInventory st
                  st2 = st { playerInventory = newInv }
                  msg = "Dodano przedmiot " ++ item
              in (st2, msg)
            Nothing ->
              (st, "Error")

checkIfTooGreedyPure :: Int -> GameState -> GameState
checkIfTooGreedyPure c st =
  if c == 4
    then st { playerLocation = AlejkaAlkohol, currentPhase = Phase2 }
    else st

pickUpItem :: String -> Game ()
pickUpItem item = do
  st <- get
  let (st2, msg) = addItemInternal item st
      st3 = st2 { positionedItems = filter (\p -> itemName p /= item || itemLocation p /= playerLocation st) (positionedItems st2) }
  put st3
  liftIO $ putStrLn msg
  checkIfTooGreedy (moneyItemTaken st3)

invite :: String -> Game ()
invite "rafalek" = do
  st <- get
  let gl = guestList st
      gl2 = "rafalek" : "martynka" : gl
      c = guestsCount st + 2
      st2 = st { guestList = gl2, guestsCount = c, npcs = filter (\npc -> npcName npc /= "martynka" || npcLocation npc /= SalaWykladowa) (npcs st) }
  put st2

invite "martynka" = do
  st <- get
  let gl = guestList st
      gl2 = "rafalek" : "martynka" : gl
      c = guestsCount st + 2
      st2 = st { guestList = gl2, guestsCount = c, npcs = filter (\npc -> npcName npc /= "rafalek" || npcLocation npc /= LaboratoriumSieciowe) (npcs st) }
  put st2

invite guestName = do
  st <- get
  let gl = guestList st
      gl2 = guestName : gl
      c = guestsCount st + 1
      st2 = st { guestList = gl2, guestsCount = c }
  put st2

fromJustPrezent :: String -> Maybe (String, String, String, String)
fromJustPrezent name = find (\(n,_,_,_) -> n == name) allPrezenty

checkIfHasItem :: String -> [String] -> Bool
checkIfHasItem x inv = x `elem` inv

addItemToPlayerInventory :: String -> GameState -> GameState
addItemToPlayerInventory item st =
  let (st2, _) = addItemInternal item st
  in st2

lookupWeakItem :: String -> Maybe String
lookupWeakItem npcName =
  (\(_, _, weak, _) -> weak) <$> find (\(n, _, _, _) -> n == npcName) allPrezenty

spawnBetterNpc :: String -> GameState -> ([String], [(String, Location)], GameState)
spawnBetterNpc npcName st =
  case fromJustPrezent npcName of
    Just (_, expected, _, strong) ->
      let has = checkIfHasItem expected (partyInventory st)
      in if has
         then ([npcName], [], addItemToPlayerInventory strong st)
         else
           let maybeWeak = lookupWeakItem npcName
           in case maybeWeak of
                Just weak -> ([npcName], [], addItemToPlayerInventory weak st)
                Nothing -> ([npcName], [], st)
    Nothing -> ([npcName], [], st)

spawnWorseNpc :: String -> GameState -> ([String], [(String, Location)], GameState)
spawnWorseNpc npcName st =
  case find (\(n,_,_,_) -> n == npcName) allPrezenty of
    Just (_, expected, weak, _) ->
      let has = checkIfHasItem expected (partyInventory st)
      in if has
         then ([npcName], [], addItemToPlayerInventory weak st)
         else ([npcName], [], st)
    Nothing -> ([npcName], [], st)

spawnBetterLoop :: [String] -> GameState -> ([String], [(String, Location)], GameState)
spawnBetterLoop [] st = ([], [], st)
spawnBetterLoop (g:gs) st =
  let lover = g `elem` ["rafalek", "martynka"]
      loc = if lover then EpickaLazienka else Parkiet
      (w1, _, st2) = spawnBetterNpc g st
      (gsW1, gsW2, st3) = spawnBetterLoop gs st2
  in (g : gsW1, (g, loc) : gsW2, st3)

spawnWorseLoop :: [String] -> GameState -> ([String], [(String, Location)], GameState)
spawnWorseLoop [] st = ([], [], st)
spawnWorseLoop (g:gs) st =
  let lover = g `elem` ["rafalek", "martynka"]
      loc = if lover then EpickaLazienka else Parkiet
      (w1, _, st2) = spawnWorseNpc g st
      (gsW1, gsW2, st3) = spawnWorseLoop gs st2
  in (g : gsW1, (g, loc) : gsW2, st3)

spawnNpcs :: Double -> [String] -> GameState -> ([String], [(String, Location)], GameState)
spawnNpcs quality guests st =
  if quality > 1
    then spawnBetterLoop guests st
    else spawnWorseLoop guests st

talkTo :: String -> Game ()
talkTo npcName = do
  st <- get
  let ph = currentPhase st
  msg <- dialogFor npcName ph
  unless (null msg) (liftIO $ putStrLn msg)

initFight :: GameState -> (GameState, [String])
initFight st =
  let st2 = st { inFight = True }
      intro = [ "Marek: Andże? Co ty tutaj robisz?"
              , "Andże: Buhahaha! ... Szykuj się na zniszczenie domówki!"
              , "Marek: Niedobrze. Będę musiał sobie z nim jakoś poradzić."
              , "Atak: use 'Przedmiot'"
              ]
  in (st2, intro)

fightCheck :: GameState -> IO [String]
fightCheck st = do
  let ehp = enemyHp st
      php = playerHp st
      inv = playerInventory st
  if ehp <= 0
    then return ["Po długiej walce, Andże pada...", "(Zakończenie pozytywne)"]
    else if php <= 0 || null inv
         then return ["Marek pada jak menel...", "(Zakończenie negatywne)"]
         else return ["Twoje HP: " ++ show php, "HP Andżeja: " ++ show ehp, "Twoje przedmioty: " ++ show inv]

fightIteration :: Int -> Int -> GameState -> IO GameState
fightIteration heal dmg st = do
  let newPlayerHP = playerHp st + heal
      newEnemyHP = enemyHp st - dmg
      edamage = 10
      finalPlayerHP = newPlayerHP - edamage
  putStrLn $ "Andże atakuje i zadaje " ++ show edamage ++ " obrażeń."
  let st2 = st { playerHp = finalPlayerHP, enemyHp = newEnemyHP }
  return st2

useItem :: String -> Game ()
useItem item = do
  st <- get
  if not (inFight st)
    then liftIO $ putStrLn "Komenda tylko w walce"
    else if item `elem` playerInventory st
      then case find (\f -> fName f == item) allFightItems of
        Just fi -> do
          let heal = fHeal fi
              dmg = fDamage fi
              inv = delete item (playerInventory st)
              st2 = st { playerInventory = inv }
          liftIO $ putStrLn $ "Użyłeś: " ++ item
          st3 <- liftIO $ fightIteration heal dmg st2
          put st3
          liftIO $ putStrLn "OK"
        Nothing -> liftIO $ putStrLn "Nie możesz użyć tego przedmiotu."
      else liftIO $ putStrLn "Nie możesz użyć tego przedmiotu."
