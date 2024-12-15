{-# LANGUAGE LambdaCase #-}

module PhaseDialogues (phaseDialogue) where

import GameState
import Data.Map (Map)
import qualified Data.Map as Map

-- | Mapa faz gry na dialogi
phaseDialogues :: Map GameStatePhase [String]
phaseDialogues = Map.fromList
    [ (Phase0, [""])
    , (Phase1, ["Muszę zarobić trochę kasy, żeby zorganizować najlepszą domóweczkę na Targówku."])
    , (Phase2, ["Dobra, mam już trochę kasy. Teraz muszę zrobić zakupy na imprezę. Idę do Makro."])
    , (Phase3, ["Teraz chyba mam jakiś wykład... ale jednak wolę zaprosić ludzi na domówkę."])
    , (Phase4, ["Mam nadzieję, że było warto. Zaczynamy imprezę!"])
    , (Phase5, [""])
    ]

-- | Wyświetla dialog dla danej fazy
phaseDialogue :: GameStatePhase -> IO ()
phaseDialogue phase = case Map.lookup phase phaseDialogues of
    Just msgs -> mapM_ putStrLn msgs
    Nothing   -> return ()