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
  ("krzemarz", 1) -> addMoney 10 "- Marek: Siema Krzemarz! Dawno cię nie widziałem. Chyba ten system cię oszukał?\n\
                          \- Krzemarz: Siema Marek! Słuchaj, tak dawno nie prałem moich ubrań proszkiem Perwoll, że już chyba nie wytrzymam. Masz może trochę?\n\
                          \- Marek: Jasne, żaden problem. 10 złotych i jest twój.\n\
                          \- Krzemarz: 10 złotych??? Drogo. No ale dobra, biorę. Trzymaj."
  ("jacuś", 1)     -> addMoney 15 "- Marek: Cześć Jacuś! Nie masz może ochoty na jakiegoś ptysia?\n\
                          \- Jacuś: Ty to jednak mnie znasz! Słuchaj, ptysie to tylko w zimie, teraz potrzebny mi proszek Vizir. Mam 15 złotych, uda się coś załatwić?\n\
                          \- Marek: Spokojnie, damy radę. Trzymaj."
  ("sobix", 1)     -> addMoney 5  "- Marek: Sobix! Jak tam twoja sieć kontaktów? Znasz może kogoś, kto potrzebuje jakiegoś proszku?\n\
                          \- Sobix: Oj tak. Znam, i jestem to ja. Daj mi trochę, zrobię wszystko, żeby wyprać moją koszulę.\n\
                          \- Marek: Oczywiście, mogę ci trochę odpalić za 5 złotych. Pasuje ci?\n\
                          \- Sobix: To wszystko co mam, ale chyba warto. Trzymaj."
  ("trąba", 1)     -> addMoney 20 "- Marek: O, Trąba. Ty to pewnie będziesz chciał jakiś konkret?\n\
                          \- Trąba: A żebyś wiedział. Mam 20 złotych, chcę ten co zwykle, o zapachu lawendy.\n\
                          \- Marek: Żaden problem, jest twój."
  ("królik", 1)    -> addMoney 15 "- Marek: Cześć Królik! Nie brakuje ci ostatnio proszku?\n\
                          \- Królik: Brakuje, brakuje. Bez proszku czuję się jak bez ręki. Masz trochę? Za jakieś 15 złotych?\n\
                          \- Marek: Oczywiście, że mam. Trzymaj."
  ("stróżyk", 1)   -> removeMoney 50 "- Stróżyk: Tak naprawdę mogliby Państwo nic ode mnie nie dostać, to nie jest szkoła gdzie pani pisze coś na tablicy a Pan się uczy z zeszytu, wskazuję tematy które będą obowiązywać, jeśli ktoś nawet nie wie jak się oznacza najprostsze rzeczy, to nie moja sprawa, taka osoba ma obowiązek sama opanować ten materiał, to nie jest obowiązkowe a ja nie mam obowiązku niczego udostępniać, więc dobrze by było gdyby Pan pohamował swoje roszczenia, od zawsze było tak że końcowa część materiału nie jest omawiana, ten materiał obowiązuje bo to ma wymagać zaangażowania od Państwa, wszystko już zostało omówione, zestawy zadań też już są, więc może Pan się do tego zabrać tak jak powinno to wyglądać. Btw wyskakuj z portfolio.\n\
                          \- Marek: Eeeee… no nie wiem, to nie brzmi zbyt dobrze…\n\
                          \- Stróżyk: *Zabiera Markowi 50 złotych*"
  ("wituś", 1)     -> removeMoney 30 "- Marek: Siema Wituś! Nie męczą cię te twoje filmiki?\n\
                          \- Wituś: Jedyne co mnie męczy, to twoja morda. Dawaj hajs i będzie git. *Zabiera Markowi 30zł*"
  ("kożuch", 1)    -> addMoney 30 "- Marek: Cześć Kożuch! Słyszałem, że ostatnio miałeś spore wzmożenie dydaktyczne?\n\
                          \- Kożuch: Miałem, miałem. A jedyne, czego nie mam, to Perwoll. Daj trochę.\n\
                          \- Marek: Jasne, chętnie. Ile potrzebujesz?\n\
                          \- Kożuch: za 30 złotych.\n\
                          \- Marek: Uuu, sporo. Jest twój."
  ("świeczka", 1)  -> addMoney 15 "- Marek: Świeczka! Prawdopodobnie będziesz chciał trochę Persila?\n\
                          \- Świeczka: Nie Persila, tylko Perwolla. Daj go za 15 złotych.\n\
                          \- Marek: Ok, trzymaj."
  ("śliwka", 1)    -> addMoney 20 "- Marek: Śliwka, miło cię widzieć! Słyszałem, że dużo ostatnio chodzisz na ryby?\n\
                          \- Śliwka: Tak, to prawda. Ostatnio zarzucałem sieci i wpadłem takie bagno, że do tej pory nie doprałem moich spodni. Masz trochę Vizira? Mam przy sobie 20 złotych.\n\
                          \- Marek: Jasne, powodzenia."
  ("król_julian", 1) -> addMoney 10 "- Marek: Król Julian! Podobno tak ci brakuje Persila, że prawie zrobiła ci się papka z mózgu i śnisz na jawie. To prawda?\n\
                          \- Król Julian: Mrrrrrrr\n\
                          \- Marek: Tak myślałem, powalona Idea. Trzymaj proszek, tylko odpal mi z 10 złotych."
  ("duży_eryk", 1)    -> removeMoney 40 "- Duży Eryk: Duży Eryk: Słyszałem, że ostatnio wymieniałeś okna na coś lepszego debilu? Dawaj hajs."
  ("biały_crook", 1)   -> addMoney 50 "- Marek: Siema Crooku! Potrzebujesz czegoś?\n\
                          \- Biały Crook: Tak, przydałoby się trochę Persila, będziesz miał za 50 złotych?\n\
                          \- Marek: Jasne, trzymaj. W końcu odpowiadasz za bezpieczeństwo całego systemu."
  ("chudy", 1)         -> addMoney 25 "- Marek: Ale nudy, szkoda mi czasu.\n\
                          \- Chudy: Nie, zaczekaj! Zapłacę ci.\n\
                          \- Marek: 25 złotych?\n\
                          \- Chudy: Może być."
  ("jeziorak", 1)      -> addMoney 20 "- Marek: Cześć Jeziorak! Słyszałem, że ostatnio byłeś w Azji?\n\
                          \- Jeziorak: Tak, cieszę się, że mnie wołałeś. Potrzebny mi cały stos Vizira.\n\
                          \- Marek: Jasne, mogę ci dać, za 20 złotych.\n\
                          \- Jeziorak: Chwila, chwila. Pan mnie nie oszukuje? A nie, wszystko w porządku."
  ("niespodzianka", 1) -> removeMoney 50 "- Niespodzianka: Niespodzianka! Wyskakuj z hajsu leszczu."
  ("charkacz", 1)      -> addMoney 10 "- Marek: Hej!\n\
                          \- Charkacz: …\n\
                          \- Marek: Aha. Chcesz może Perwoll?\n\
                          \- Charkacz: Tak. 10zł."
  ("mariusz", 1)       -> addMoney 15 "- Marek: Cześć! Potrzebujesz czegoś?\n\
                          \- Mariusz: Tak, chciałbym zmarnować ci trochę czasu.\n\
                          \- Marek: No nie wiem, a chcesz może Persil?\n\
                          \- Mariusz: Ty to wiesz, jak przekonać człowieka. Trzymaj 15zł."
  ("grabie", 1)        -> addMoney 15 "- Marek: Siema Grabie! Widzę, że znowu bierzesz się za wszystko jednocześnie?\n\
                          \- Grabie: Tak, czemu nie. Ale potrzebny mi będzie Vizir. Masz trochę?\n\
                          \- Marek: Jasne, trzymaj za 15zł."
  ("gracjan", 1)       -> removeMoney 50 "- Marek: Gracjan! Czemu znowu masz przy sobie identyfikator ze swojej pracy?\n\
                          \- Gracjan: Żebyś nie zauważył, jak zabrałem ci 50 złotych."

  -- Dialogi dla fazy 3
  ("karol_z_wietnamu", 3) -> return "- Marek: Siema Karol! Wbijasz na domówkę czy wolisz cisnąć AMPPZty?\n\
                                \- Karol z Wietnamu: Wiesz co, wolałbym pić piwo i jeść pizzę na fakturę. Ale i tak przyjdę!\n\
                                \- Marek: Super. W takim razie sobota wieczorem, znasz adres."
  ("martynka", 3)       -> return "- Marek: Martynka! Chcesz przyjść na domówkę?\n\
                              \- Martynka: Tak, ale chcę cały czas kuć do kolosa z SOI na za tydzień i jeść pizzę hawajską.\n\
                              \- Marek: No dobra, tylko nie zapomnij czegoś ze sobą zabrać."
  ("kopytek", 3)        -> return "- Marek: Siema Kopytek! Nie chcesz może wbić na domóweczkę w ten weekend?\n\
                              \- Kopytek: No nie wiem, nie wiem. A będę mógł tam obejrzeć anime i się pouczyć?\n\
                              \- Marek: Tak, tak, będziesz mógł. Zabierz tylko coś do picia."
  ("piotrek", 3)        -> return "- Marek: Piotrek, przychodzisz na domówkę? Będzie jeszcze lepsza niż ostatni meczyk u ciebie.\n\
                              \- Piotrek: A wiesz co, czemu nie. I tak nie mam nic do roboty.\n\
                              \- Marek: Najs."
  ("bartek", 3)         -> return "- Marek: Wbijasz w weekend na domóweczkę? Może nawet kupię ci piwo kraftowe.\n\
                              \- Bartek: Ty to wiesz jak mnie przekonać. Pewnie, że będę."
  ("jureczek", 3)       -> return "- Marek: Siema Jureczek! Dawaj na domówkę w ten weekend.\n\
                              \- Jureczek: Spoko, czemu nie."
  ("tomek", 3)          -> return "- Marek: Tomek! Chcesz przyjść na domówkę w weekend?\n\
                              \- Tomek: Jasne, chętnie. Zabiorę coś ze sobą."
  ("wojtek", 3)         -> return "- Marek: Wojtek, chcesz wbić na domówkę?\n\
                              \- Wojtek: Jasne, czemu nie, wezmę ze sobą papierek i ciasteczka. Kiedy ma być?\n\
                              \- Marek: W sobotę wieczorem, u mnie.\n\
                              \- Wojtek: Idealnie! W takim razie w sobotę się widzimy."
  ("maciuś", 3)         -> return "- Marek: Maciuś, masz rację. Piszesz kod i wbijaj na domówkę.\n\
                              \- Maciuś: No dobra, to może nawet coś ze sobą wezmę, na przykład FFT. Ale i tak będziemy robić zadanka. A btw, myślałeś o pracy w Visie?"
  ("olek", 3)           -> return "- Marek: Chcesz przyjść na domówkę?\n\
                              \- Olek: A chcesz pogadać o drzewach przedziałowych?\n\
                              \- Marek: Eee… wiesz co, może kiedy indziej.\n\
                              \- Olek: Szkoda. Ale na domówkę i tak przyjdę."
  ("kubuś", 3)          -> return "- Marek: Siema Kubuś, wbijasz na domóweczkę w sobotę wieczorem?\n\
                              \- Kubuś: Mogę wbić, tylko dokończę pisać projekt z prologa na PARP. Ale 1. Będzie tam jager, 2. Nie idę spać przez ostatnie 3 dni. Pasuje ci to?\n\
                              \- Marek: Git. To widzimy się w sobotę."
  ("rafałek", 3)        -> return "- Marek: Rafałek! Wbijasz na domówkę?\n\
                              \- Rafałek: Kurde, wbiłbym, ale trzeba zrobić jakąś kampanię marketingową.\n\
                              \- Marek: Dobra, klienci mogą zaczekać, zrobisz kampanię dla domówki. Robimy chlanie.\n\
                              \- Rafałek: No dobra. To zabieram jakieś dobre picie i odpalamy domóweczkę."
  ("łukasz_2", 3)       -> return "- Marek: Siema Łukasz? Mówił ci ktoś, że wyglądasz jak Łukasz i tak samo się nazywasz?\n\
                              \- Łukasz: Dobra, we. O co chodzi?\n\
                              \- Marek: Wbijasz na domówkę w sobotę wieczorem?\n\
                              \- Łukasz: Jasne. To wezmę ze sobą jakieś 0.5."
  ("krzysiu", 3)        -> return "- Marek: Wbijasz na domówkę zachlać mordę?\n\
                              \- Krzysiu: Uuuuu, już to zoeobłem. Ale jaks ccheszsz , to smo żemy to powtózyć. Mogę znowu kręcić driny.\n\
                              \- Marek: Git. W takim razie widzimy się w sobotę."
  ("mikołaj", 3)        -> return "- Marek: Dawaj na domóweczkę, napijesz się i będziesz robił pompki.\n\
                              \- Mikołaj: No dobra, ale pamiętaj, że ja nie piję. Masa sama się nie zrobi na siłce.\n\
                              \- Marek: Spoko, jasne, damy radę."
  ("jasiulek", 3)       -> return "- Marek: Jasiulek, dawaj chlanie u mnie w sobotę.\n\
                              \- Jasiulek: Kuuurde, oczywiście, że tak. Wbijam, ale będziecie mnie holować do domu."
  ("mati", 3)           -> return "- Marek: Mati, wbijasz na domóweczkę. Tylko tym razem pamiętaj, że masz dziewczynę xD.\n\
                              \- Mati: Aha, bardzo zabawne. Wbijam, tym razem postaram się przeżyć do końca."
  ("wiktor", 3)         -> return "- Marek: Siema Wiktor, wbijaj na domóweczkę.\n\
                              \- Wiktor: Spoko, chętnie wbiję.\n\
                              \- Marek: Giiit. Tylko tym razem coś wypij xD.\n\
                              \- Wiktor: Jasne, tym razem nie przyjadę samochodem xD."

  -- Dialogi dla fazy 4
  ("adrian", 4)         -> return "- Marek: Adrian! Myślałem, że przyjechałeś TTką, czemu leżysz nad kiblem.\n\
                              \- Adrian: Blebuegrrbłłl."
  -- Domyślny dialog podczas domówki (phase4)
  (_, 4)                -> return "- Marek: I jak się bawisz? Warto było wbić na domóweczkę?\n\
                              \- Jest zajebiście!"

  -- Dialog dla babci w fazie 1
  ("babcia", 1)         -> addMoney 50 "- Babcia: Masz wnusiu 50 zł na alkohol"

  -- Domyślny dialog dla innych przypadków
  _                     -> return ""


debugState :: StateT GameState IO ()
debugState = do
  st <- get
  liftIO $ do
    putStrLn "========== DEBUGGING GAME STATE =========="
    putStrLn $ "Player Location: " ++ show (playerLocation st)
    putStrLn $ "Player Inventory: " ++ show (playerInventory st)
    putStrLn $ "Party Inventory: " ++ show (partyInventory st)
    putStrLn $ "Player Money: " ++ show (playerMoney st)
    putStrLn $ "Guest List: " ++ show (guestList st)
    putStrLn $ "Current Phase: " ++ show (currentPhase st)
    putStrLn $ "Party Quality: " ++ show (partyQuality st)
    putStrLn $ "Guests Count: " ++ show (guestsCount st)
    putStrLn $ "In Fight: " ++ show (inFight st)
    putStrLn $ "Player HP: " ++ show (playerHp st)
    putStrLn $ "Enemy HP: " ++ show (enemyHp st)
    putStrLn $ "Money Items Taken: " ++ show (moneyItemTaken st)
    putStrLn "=========================================="


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