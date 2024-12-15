{-# LANGUAGE LambdaCase #-}

module PhaseDialogues (phaseDialogue) where

import GameState
import Data.Map (Map)
import qualified Data.Map as Map

-- | Mapa faz gry na dialogi
phaseDialogues :: Map GameStatePhase [String]
phaseDialogues = Map.fromList
    [ (Phase0, ["Rozpoczynasz grę. Przygotuj się na epicką domówkę!"])
    , (Phase1, ["Faza 1: Zbierasz przedmioty i planujesz imprezę."])
    , (Phase2, ["Faza 2: Organizujesz miejsce i zapraszasz gości."])
    , (Phase3, ["Faza 3: Impreza się zaczyna!"])
    , (Phase4, ["Faza 4: Domówka jest w pełni rozkręcona."])
    , (Phase5, ["Faza 5: Zakończenie imprezy. Dziękujemy za grę!"])
    ]

-- | Wyświetla dialog dla danej fazy
phaseDialogue :: GameStatePhase -> IO ()
phaseDialogue phase = case Map.lookup phase phaseDialogues of
    Just msgs -> mapM_ putStrLn msgs
    Nothing   -> return ()
