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

checkIfTooGreedyPure :: Int -> GameState -> GameState
checkIfTooGreedyPure c st =
  if c == 4
    then st { playerLocation = AlejkaAlkohol, currentPhase = Phase2 }
    else st

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

pickUpItem :: String -> Game ()
pickUpItem item = do
  st <- get
  let (st2, msg) = addItemInternal item st
      st3 = st2 { positionedItems = filter (\p -> itemName p /= item || itemLocation p /= playerLocation st) (positionedItems st2) }
  put st3
  liftIO $ putStrLn msg
  checkIfTooGreedy (moneyItemTaken st3)

invite :: String -> Game ()
invite "rafałek" = do
  st <- get
  let gl = guestList st
      gl2 = "rafałek" : "martynka" : gl
      c = guestsCount st + 2
      st2 = st { guestList = gl2, guestsCount = c
               , npcs = filter (\npc -> not ((npcName npc == "martynka" && npcLocation npc == SalaWykladowa) ||
                                             (npcName npc == "rafałek" && npcLocation npc == LaboratoriumSieciowe))) (npcs st)}
  put st2

invite "martynka" = do
  st <- get
  let gl = guestList st
      gl2 = "rafałek" : "martynka" : gl
      c = guestsCount st + 2
      st2 = st { guestList = gl2, guestsCount = c
               , npcs = filter (\npc -> not ((npcName npc == "rafałek" && npcLocation npc == LaboratoriumSieciowe) ||
                                             (npcName npc == "martynka" && npcLocation npc == SalaWykladowa))) (npcs st)}
  put st2

invite guestName = do
  st <- get
  let gl = guestList st
      gl2 = guestName : gl
      c = guestsCount st + 1
      st2 = st { guestList = gl2, guestsCount = c
               , npcs = filter (\npc -> not (npcName npc == guestName)) (npcs st)}
  put st2

fromJustPrezent :: String -> Maybe (String, String, String, String)
fromJustPrezent name = find (\(n,_,_,_) -> n == name) allPrezenty

checkIfHasItem :: String -> [String] -> Bool
checkIfHasItem x inv = x `elem` inv

addItemToPlayerInventory :: String -> GameState -> GameState
addItemToPlayerInventory item st =
  let (st2,_) = addItemInternal item st
  in st2

lookupWeakItem :: String -> Maybe String
lookupWeakItem npcName =
  (\(_, _, weak, _) -> weak) <$> find (\(n, _, _, _) -> n == npcName) allPrezenty

lookupStrongItem :: String -> Maybe String
lookupStrongItem npcName =
  (\(_, _, _, strong)-> strong) <$> find (\(n,_,_,_) -> n == npcName) allPrezenty

lookupExpectedItem :: String -> Maybe String
lookupExpectedItem npcName =
  (\(_, expected,_,_)-> expected) <$> find (\(n,_,_,_) -> n == npcName) allPrezenty

giveItem :: String -> GameState -> GameState
giveItem it st =
  case find (\f->fName f==it) allFightItems of
    Just _ -> st{playerInventory = it : playerInventory st}
    Nothing -> st

spawnBetterNpc :: String -> Double -> [String] -> GameState -> ([String],[(String,Location)],GameState)
spawnBetterNpc npcName q partyInv st =
  case fromJustPrezent npcName of
    Just (_, expected, weak, strong) ->
      let hasItem = checkIfHasItem expected partyInv
          goodQ = q > 1
          gift = case (goodQ,hasItem) of
                   (True,True) -> Just strong
                   (True,False) -> Just weak
                   (False,True) -> Just weak
                   (False,False) -> Nothing
          lover = npcName `elem` ["rafałek","martynka"]
          loc = if lover then EpickaLazienka else Parkiet
          st2 = case gift of
                  Just g -> giveItem g st
                  Nothing -> st
      in ([npcName],[(npcName,loc)], st2)
    Nothing -> ([npcName],[(npcName,Parkiet)],st)

spawnWorseNpc :: String -> Double -> [String] -> GameState -> ([String],[(String,Location)],GameState)
spawnWorseNpc npcName q partyInv st =
  case fromJustPrezent npcName of
    Just (_, expected, weak, strong) ->
      let hasItem = checkIfHasItem expected partyInv
          goodQ = q > 1
          gift = case (goodQ,hasItem) of
                   (True,True) -> Just strong
                   (True,False) -> Just weak
                   (False,True) -> Just weak
                   (False,False) -> Nothing
          lover = npcName `elem` ["rafałek","martynka"]
          loc = if lover then EpickaLazienka else Parkiet
          st2 = case gift of
                  Just g -> giveItem g st
                  Nothing -> st
      in ([npcName],[(npcName,loc)],st2)
    Nothing ->
      let lover = npcName `elem` ["rafałek","martynka"]
          loc = if lover then EpickaLazienka else Parkiet
      in ([npcName],[(npcName,loc)],st)

spawnBetterLoop :: Double -> [String] -> GameState -> ([String],[(String,Location)],GameState)
spawnBetterLoop _ [] st = ([],[],st)
spawnBetterLoop q (g:gs) st =
  let (w1,w2,st2) = spawnBetterNpc g q (partyInventory st) st
      (gsW1, gsW2, st3) = spawnBetterLoop q gs st2
  in (g:gsW1, w2++gsW2, st3)

spawnWorseLoop :: Double -> [String] -> GameState -> ([String],[(String,Location)],GameState)
spawnWorseLoop _ [] st = ([],[],st)
spawnWorseLoop q (g:gs) st =
  let (w1,w2,st2) = spawnWorseNpc g q (partyInventory st) st
      (gsW1, gsW2, st3) = spawnWorseLoop q gs st2
  in (g:gsW1, w2++gsW2, st3)

spawnNpcs :: Double -> [String] -> GameState -> ([String],[(String,Location)],GameState)
spawnNpcs quality guests st =
  if quality > 1
    then spawnBetterLoop quality guests st
    else spawnWorseLoop quality guests st

talkTo :: String -> Game ()
talkTo npcTargetName = do
  st <- get
  let ph = currentPhase st
  msg <- dialogFor npcTargetName ph
  unless (null msg) $ do
    liftIO $ putStrLn msg
    st' <- get
    put st'{ npcs = filter (\npc -> npcName npc /= npcTargetName) (npcs st') }
    liftIO $ putStrLn $ npcTargetName ++ " gdzieś idzie."
    when (ph == Phase3) $ invite npcTargetName

initFight :: GameState -> (GameState, [String])
initFight st =
  let st2 = st { inFight = True }
      intro = [ "Marek: Andżej? Co ty tutaj robisz?"
              , "Andżej: Buhahaha! ... Szykuj się na zniszczenie domówki!"
              , "Marek: Niedobrze. Będę musiał sobie z nim jakoś poradzić."
              , "Atak: use 'Przedmiot'"
              ]
  in (st2, intro)

describeInventoryQ :: GameState -> IO ()
describeInventoryQ st = do
  displayInventoryQ st

displayInventoryQ :: GameState -> IO ()
displayInventoryQ st = do
  let inventory = playerInventory st
  if null inventory
    then putStrLn "Twój ekwipunek jest pusty."
    else do
      putStrLn "Twój ekwipunek zawiera:"
      mapM_ (putStrLn . ("  -> " ++)) inventory

fightCheck :: GameState -> IO ()
fightCheck st = do
  let ehp = enemyHp st
      php = playerHp st
      inv = playerInventory st
  if ehp <= 0
    then putStrLn "Po długiej i wymagającej walce, Andżej traci przytomność i jak menel leży na ziemi. Wszyscy na domówce, jeśli jeszcze kontaktują, zaczynają świętować wielkie zwycięstwo Marka. W końcu równowaga na Targówku została przywrócona. Marek zdobył szacunek na dzielni rozprawiając się z Andżejem, a Andżej został zesłany na Białołękę (albo nawet Wawer - ale nikt nie wie, bo nikt nigdy tam nie dotarł). Dziękujemy za przejście gry i widzimy się w najbliższym DLC, napisanym w Smalltalku!"
    else if php <= 0 || null inv
         then putStrLn "Po długiej i wymagającej walce, Marek traci przytomność i jak menel leży na ziemi. Wszyscy na domówce, jeśli jeszcze kontaktują, są przerażeni wynikiem walki i nikt nie może być pewien, co go czeka... Równowaga na Targówku została zaburzona. Andżej przejął władzę na Targówku, na Wydziale i w prawie całej Warszawie. Jedyna nadzieja w Śródmieściu i Grochowie, gdzie zaczyna powstawać ruch oporu. Dziękujemy za przejście gry i widzimy się w najbliższym DLC, napisanym w Haskellu!"
         else do
           putStrLn $ "Twoje HP: " ++ show php
           putStrLn $ "HP Andżeja: " ++ show ehp
           describeInventoryQ st


fightIteration :: Int -> Int -> GameState -> IO GameState
fightIteration heal dmg st = do
  let newPlayerHP = playerHp st + heal
      newEnemyHP = enemyHp st - dmg
      edamage = 10
      finalPlayerHP = newPlayerHP - edamage
  putStrLn $ "Andżej atakuje i zadaje " ++ show edamage ++ " obrażeń."
  let st2 = st { playerHp = finalPlayerHP, enemyHp = newEnemyHP }
  return st2

useItem :: String -> Game ()
useItem item = do
  st <- get
  if not (inFight st)
    then liftIO $ putStrLn "Komenda działa tylko w walce"
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


