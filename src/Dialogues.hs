{-# LANGUAGE LambdaCase #-}

module Dialogues (dialogFor, toPhaseInt) where

import GameState
import Control.Monad.State

-- | Konwertuje GameStatePhase na Int, pomocne dla dialogFor
toPhaseInt :: GameStatePhase -> Int
toPhaseInt = \case
  Phase0 -> 0
  Phase1 -> 1
  Phase2 -> 2
  Phase3 -> 3
  Phase4 -> 4
  Phase5 -> 5

-- | Funkcja dialogFor obsługuje dialogi NPC w zależności od nazwy i fazy gry.
-- Modyfikuje stan gry (dodawanie/odejmowanie pieniędzy) oraz zwraca odpowiedni dialog.
dialogFor :: String -> GameStatePhase -> StateT GameState IO String
dialogFor npcName phase = case (npcName, toPhaseInt phase) of
  -- Dialogi dla fazy 1
  ("krzemarz", 1) -> addMoney 10 "Marek: Siema Krzemarz! Dawno cię nie widziałem...\n...Trzymaj."
  ("jacus", 1)     -> addMoney 15 "Marek: Cześć Jacuś!...Trzymaj."
  ("sobix", 1)     -> addMoney 5  "Marek: Sobix! ...Trzymaj."
  ("traba", 1)     -> addMoney 20 "Marek: O, Trąba. ...Jest twój."
  ("krolik", 1)    -> addMoney 15 "Marek: Cześć Królik!...Trzymaj."
  ("strozyk", 1)   -> removeMoney 50 "Stróżyk: ...Btw wyskakuj z portfolio.\nMarek: ...Tracisz 50zł"
  ("witus", 1)     -> removeMoney 30 "Marek: Siema Wituś!...Dawaj hajs.\nTracisz 30zł"
  ("kozuch", 1)    -> addMoney 30 "Marek: Cześć Kożuch!...Jest twój."
  ("swieczka", 1)  -> addMoney 15 "Marek: Świeczka!...Ok, trzymaj."
  ("sliwka", 1)    -> addMoney 20 "Marek: Śliwka,... Jasne, powodzenia."
  ("krol_julian", 1) -> addMoney 10 "Marek: Król Julian!...Trzymaj proszek."
  ("duzy_eryk", 1)    -> removeMoney 40 "Duży Eryk: ...Dawaj hajs.\nTracisz 40zł"
  ("bialy_crook", 1)   -> addMoney 50 "Marek: Siema Crooku!...Jasne, trzymaj."
  ("chudy", 1)         -> addMoney 25 "Marek: Ale nudy,...Może być."
  ("jeziorak", 1)      -> addMoney 20 "Marek: Cześć Jeziorak!...Ok, jest twój."
  ("niespodzianka", 1) -> removeMoney 50 "Niespodzianka!...Wyskakuj z hajsu.\nTracisz 50zł"
  ("charkacz", 1)      -> addMoney 10 "Marek: Hej!...Trzymaj 10zł."
  ("mariusz", 1)       -> addMoney 15 "Marek: Cześć!...Trzymaj 15zł."
  ("grabie", 1)        -> addMoney 15 "Marek: Siema Grabie!...Trzymaj za 15zł."
  ("gracjan", 1)       -> removeMoney 50 "Marek: Gracjan!...Tracisz 50zł"

  -- Dialogi dla fazy 3
  ("karol_wietnam", 3) -> return "Marek: Siema Karol!... Super..."
  ("martynka", 3)       -> return "Marek: Martynka! ...tylko nie zapomnij czegoś ze sobą zabrać."
  ("kopytek", 3)        -> return "Marek: Siema Kopytek!...Zabierz coś do picia."
  ("piotrek", 3)        -> return "Marek: Piotrek,... Najs."
  ("bartek", 3)         -> return "Marek: Wbijasz... Pewnie, że będę."
  ("jureczek", 3)       -> return "Marek: Siema Jureczek!...Spoko, czemu nie."
  ("tomek", 3)          -> return "Marek: Tomek!... Jasne, chętnie."
  ("wojtek", 3)         -> return "Marek: Wojtek,... Idealnie!"
  ("macius", 3)         -> return "Maciuś: Ty, a to ostatnie zadanie... A btw, myślałeś o Visie?"
  ("olek", 3)           -> return "Marek: Chcesz przyjść?...Szkoda. Ale i tak wbijaj."
  ("kubus", 3)          -> return "Marek: Siema Kubuś,...Git. Widzimy się."
  ("rafalek", 3)        -> return "Marek: Wbijasz?... Robimy chlanie.\nRafałek: No dobra."
  ("lukasz_2", 3)       -> return "Marek: Siema Łukasz?...Jasne. Biorę 0.5."
  ("krzysiu", 3)        -> return "Marek: Wbijasz na domówkę?...Git."
  ("mikolaj", 3)        -> return "Marek: Dawaj na domóweczkę,...Spoko."
  ("jasiulek", 3)       -> return "Marek: Jasiulek, dawaj...Kuuurde, oczywiście."
  ("mati", 3)           -> return "Marek: Mati,...Wbijam."
  ("wiktor", 3)         -> return "Marek: Siema Wiktor,... Jasne."

  -- Dialogi dla fazy 4
  ("adrian", 4)         -> return "Marek: Adrian!...Blebuegrrbłłl."
  -- Domyślny dialog podczas domówki (phase4)
  (_, 4)                -> return "Marek: I jak się bawisz? Warto było wbić na domóweczkę?\nJest świetnie!"

  -- Dialog dla babci w fazie 1
  ("babcia", 1)         -> addMoney 50 "Babcia: Masz wnusiu 50 zł na alkohol"

  -- Domyślny dialog dla innych przypadków
  _                     -> return ""

-- | Dodaje określoną kwotę pieniędzy do stanu gry i zwraca komunikat.
addMoney :: Int -> String -> StateT GameState IO String
addMoney amt baseMsg = do
  st <- get
  let newMoney = playerMoney st + amt
  put st { playerMoney = newMoney }
  return (baseMsg ++ "\nOtrzymałeś " ++ show amt ++ " zł.")

-- | Odejmuje określoną kwotę pieniędzy ze stanu gry i zwraca komunikat.
removeMoney :: Int -> String -> StateT GameState IO String
removeMoney amt baseMsg = do
  st <- get
  let newMoney = playerMoney st - amt
  put st { playerMoney = newMoney }
  return (baseMsg ++ "\nStraciłeś " ++ show amt ++ " zł.")
