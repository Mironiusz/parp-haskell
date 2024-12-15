{-# LANGUAGE LambdaCase #-}

module DataItems
  ( allFightItems
  , allPartyItems
  , allMoneyItems
  , allPrezenty
  , allNPCs
  , allPositionedItems
  , connections
  , describeLocationName
  , npcMap
  , itemMap
  , Location(..)
  ) where

import GameState
import qualified Data.Map as Map

allFightItems :: [FightItem]
allFightItems =
  [ FightItem "giga_okularki" 0 55
  , FightItem "koszulka_z_amppz" 0 55
  , FightItem "fags" 10 1
  , FightItem "beer_strong_cup" 0 5
  , FightItem "metal_rod" 0 25
  , FightItem "brick" 0 15
  , FightItem "french_key" 0 8
  , FightItem "knuckle_duster" 0 5
  , FightItem "kebab" 25 0
  , FightItem "trash" 0 1
  , FightItem "baseball" 0 10
  , FightItem "kings_tulip" 0 5
  , FightItem "normal_beer" 10 0
  , FightItem "iqos" 5 0
  , FightItem "methanol" 40 0
  , FightItem "syringe" 0 15
  , FightItem "newspaper" 0 0
  , FightItem "drinking_monkey" 20 0
  , FightItem "pusta_puszka" 0 0
  , FightItem "szalik_legii" 0 10
  , FightItem "pusta_butelka" 0 0
  , FightItem "zoladkowa_gorzka" 10 5
  , FightItem "sloik" 0 0
  , FightItem "duzy_czarny_kogut" 20 15
  , FightItem "flaga_lecha" 0 0
  , FightItem "gaz_pieprzowy" 0 20
  , FightItem "opaska_do_wlosow" 0 0
  , FightItem "czapka_wpierdolka" 0 15
  , FightItem "bialko" 5 0
  , FightItem "strzykawka_ze_sterydami" 0 25
  , FightItem "zeszyt_od_matmy" 0 0
  , FightItem "karta_wzorow_na_analize" 0 15
  , FightItem "algorytm_genetyczny" 0 0
  , FightItem "siec_neuronowa" 0 30
  , FightItem "puszka_piwa" 3 0
  , FightItem "blue_curacao" 15 10
  , FightItem "puste_piwo_kraftowe" 0 0
  , FightItem "tulipan_po_zubrowce" 0 10
  , FightItem "pusta_strzykawka" 0 0
  , FightItem "pelna_strzykawka" 0 20
  , FightItem "zelda_na_nintendo_switch" 0 0
  , FightItem "rude_wlosy" 0 10
  , FightItem "pelna_puszka" 5 1
  , FightItem "karty_wzorow_na_amppzty" 0 5
  , FightItem "sajgonki_na_fakture" 20 15
  , FightItem "mleko_od_starego" 0 0
  , FightItem "jabol_w_puszce" 10 5
  , FightItem "szachy" 0 0
  , FightItem "baniak_jabola" 15 10
  , FightItem "cegla" 0 15
  , FightItem "projekt_zespolowy_1" 0 0
  , FightItem "projekt_pap" 0 15
  , FightItem "ptysie" 0 5
  , FightItem "projekt_risc_v_na_arko" 0 30
  , FightItem "suprise" 1 8
  , FightItem "szybkie_okularki" 10 0
  , FightItem "doritos" 5 0
  ]

allPartyItems :: [PartyItem]
allPartyItems =
  [ PartyItem "bison" 30 1
  , PartyItem "jungle_ghost" 20 2
  , PartyItem "jager" 50 25
  , PartyItem "harnold" 3 1
  , PartyItem "komandos" 5 2
  , PartyItem "orzech" 25 15
  , PartyItem "smerf" 10 5
  , PartyItem "pepperoni" 30 6
  , PartyItem "hawaii" 30 1
  , PartyItem "lays_solone" 10 5
  , PartyItem "lays_papryka" 10 6
  , PartyItem "doritos_ser" 20 8
  , PartyItem "cheetos_ketchup" 15 4
  , PartyItem "pepsi" 20 1
  , PartyItem "cola" 20 2
  , PartyItem "rebull_z_makro" 7 3
  , PartyItem "tiger" 6 2
  , PartyItem "kong_strong" 6 1
  , PartyItem "vizir" 30 6
  , PartyItem "persil" 30 5
  , PartyItem "perwool" 30 8
  , PartyItem "ciasteczka_smiechu" 50 10
  , PartyItem "kosmiczny_papierek" 50 40
  ]

allMoneyItems :: [MoneyItem]
allMoneyItems =
  [ MoneyItem "skarpetka" 50
  , MoneyItem "lupa" 10
  , MoneyItem "stare_zdjecie" 100
  , MoneyItem "szczeka" 100
  , MoneyItem "tabletki" 100
  , MoneyItem "telefon" 3
  ]

allPrezenty :: [(String, String, String, String)]
allPrezenty =
 [("rafalek","komandos","pusta_puszka","szalik_legii"),
  ("kubus","jager","pusta_butelka","zoladkowa_gorzka"),
  ("macius","rebull_z_makro","sloik","duzy_czarny_kogut"),
  ("krzysiu","harnold","flaga_lecha","gaz_pieprzowy"),
  ("tomek","smerf","opaska_do_wlosow","czapka_wpierdolka"),
  ("mikolaj","perwool","bialko","strzykawka_ze_sterydami"),
  ("martynka","hawaii","zeszyt_od_matmy","karta_wzorow_na_analize"),
  ("olek","jungle_ghost","algorytm_genetyczny","siec_neuronowa"),
  ("jureczek","bison","puszka_piwa","blue_curacao"),
  ("bartek","kong_strong","puste_piwo_kraftowe","tulipan_po_zubrowce"),
  ("wojtek","kosmiczny_papierek","pusta_strzykawka","pelna_strzykawka"),
  ("kopytek","lays_papryka","zelda_na_nintendo_switch","rude_wlosy"),
  ("jasiulek","harnold","pusta_puszka","pelna_puszka"),
  ("karol_wietnam","pepperoni","karty_wzorow_na_amppzty","sajgonki_na_fakture"),
  ("lukasz_2","persil","mleko_od_starego","jabol_w_puszce"),
  ("mati","ciasteczka_smiechu","szachy","baniak_jabola"),
  ("piotrek","cheetos_ketchup","cegla","baseball"),
  ("wiktor","doritos_ser","projekt_zespolowy_1","projekt_pap"),
  ("dziewczyny","orzech","ptysie","projekt_risc_v_na_arko")
 ]

allNPCs :: [NPC]
allNPCs =
  [ NPC "babcia" PokojBabci
  , NPC "krzemarz" Jorskiego
  , NPC "jacus" Jorskiego
  , NPC "sobix" Jorskiego
  , NPC "traba" Jorskiego
  , NPC "krolik" Jorskiego
  , NPC "strozyk" Jorskiego
  , NPC "witus" Jorskiego
  , NPC "kozuch" Radzyminska
  , NPC "swieczka" Radzyminska
  , NPC "krol_julian" Radzyminska
  , NPC "sliwka" Radzyminska
  , NPC "duzy_eryk" Radzyminska
  , NPC "bialy_crook" Blokowa
  , NPC "chudy" Blokowa
  , NPC "jeziorak" Blokowa
  , NPC "niespodzianka" Blokowa
  , NPC "charkacz" Ogrodnicza
  , NPC "mariusz" Ogrodnicza
  , NPC "grabie" Ogrodnicza
  , NPC "gracjan" Ogrodnicza
  , NPC "karol_wietnam" Wietnam
  , NPC "martynka" SalaWykladowa
  , NPC "kopytek" SalaWykladowa
  , NPC "piotrek" Piwnica
  , NPC "bartek" Piwnica
  , NPC "jureczek" PiwnicaPiwnicy
  , NPC "tomek" PiwnicaPiwnicy
  , NPC "wojtek" PiwnicaPiwnicy
  , NPC "macius" LaboratoriumKomputerowe
  , NPC "olek" LaboratoriumKomputerowe
  , NPC "kubus" LaboratoriumKomputerowe
  , NPC "rafalek" LaboratoriumSieciowe
  , NPC "lukasz_2" LaboratoriumSieciowe
  , NPC "krzysiu" MeskiKibel
  , NPC "mikolaj" MeskiKibel
  , NPC "jasiulek" MeskiKibel
  , NPC "mati" Korytarz1Pietro
  , NPC "wiktor" Korytarz1Pietro
  , NPC "adrian" EpickaLazienka
  ]

allPositionedItems :: [PositionedItem]
allPositionedItems =
  [ PositionedItem "giga_okularki" PokojMarka
  , PositionedItem "koszulka_z_amppz" PokojMarka

  , PositionedItem "skarpetka" PokojBabci
  , PositionedItem "lupa" PokojBabci
  , PositionedItem "stare_zdjecie" PokojBabci
  , PositionedItem "szczeka" PokojBabci
  , PositionedItem "tabletki" PokojBabci
  , PositionedItem "telefon" PokojBabci

  , PositionedItem "fags" Jorskiego
  , PositionedItem "beer_strong_cup" Jorskiego
  , PositionedItem "metal_rod" Jorskiego
  , PositionedItem "brick" Jorskiego
  , PositionedItem "french_key" Jorskiego
  , PositionedItem "knuckle_duster" Jorskiego
  , PositionedItem "kebab" Jorskiego
  , PositionedItem "trash" Jorskiego
  , PositionedItem "baseball" Jorskiego
  , PositionedItem "kings_tulip" Jorskiego
  , PositionedItem "normal_beer" Jorskiego
  , PositionedItem "doritos" Jorskiego

  , PositionedItem "redbull" Ogrodnicza
  , PositionedItem "syringe" Ogrodnicza
  , PositionedItem "newspaper" Ogrodnicza
  , PositionedItem "drinking_monkey" Ogrodnicza

  , PositionedItem "bison" AlejkaAlkohol
  , PositionedItem "jungle_ghost" AlejkaAlkohol
  , PositionedItem "jager" AlejkaAlkohol
  , PositionedItem "harnold" AlejkaAlkohol
  , PositionedItem "komandos" AlejkaAlkohol
  , PositionedItem "orzech" AlejkaAlkohol
  , PositionedItem "smerf" AlejkaAlkohol

  , PositionedItem "pepperoni" AlejkaJedzenie
  , PositionedItem "hawaii" AlejkaJedzenie
  , PositionedItem "lays_solone" AlejkaJedzenie
  , PositionedItem "lays_papryka" AlejkaJedzenie
  , PositionedItem "doritos_ser" AlejkaJedzenie
  , PositionedItem "cheetos_ketchup" AlejkaJedzenie

  , PositionedItem "pepsi" AlejkaNapoje
  , PositionedItem "cola" AlejkaNapoje
  , PositionedItem "rebull_z_makro" AlejkaNapoje
  , PositionedItem "tiger" AlejkaNapoje
  , PositionedItem "kong_strong" AlejkaNapoje

  , PositionedItem "vizir" PalarniaSmietnik
  , PositionedItem "persil" PalarniaSmietnik
  , PositionedItem "perwool" PalarniaSmietnik
  , PositionedItem "ciasteczka_smiechu" PalarniaSmietnik
  , PositionedItem "kosmiczny_papierek" PalarniaSmietnik

  , PositionedItem "suprise" EpickaLazienka
  , PositionedItem "szybkie_okularki" Parkiet
  ]

connections :: [(Location, Location, String)]
connections =
  [ (PokojMarka, PokojBabci, "east")
  , (PokojMarka, Targowek, "south")
  , (PokojBabci, Makro, "south")
  , (Jorskiego, Radzyminska, "south")
  , (Jorskiego, Blokowa, "west")
  , (Radzyminska, Jorskiego, "north")
  , (Radzyminska, Blokowa, "west")
  , (Blokowa, Jorskiego, "east")
  , (Blokowa, Radzyminska, "south")
  , (Blokowa, Ogrodnicza, "north")
  , (Ogrodnicza, Blokowa, "south")
  , (Ogrodnicza, Makro, "north")
  , (AlejkaAlkohol, AlejkaNapoje, "west")
  , (AlejkaAlkohol, AlejkaJedzenie, "east")
  , (AlejkaAlkohol, PalarniaSmietnik, "south")
  , (AlejkaJedzenie, AlejkaAlkohol, "west")
  , (AlejkaJedzenie, AlejkaNapoje, "south")
  , (AlejkaJedzenie, PalarniaSmietnik, "north")
  , (AlejkaNapoje, AlejkaAlkohol, "north")
  , (AlejkaNapoje, AlejkaJedzenie, "south")
  , (AlejkaNapoje, PalarniaSmietnik, "west")
  , (PalarniaSmietnik, AlejkaAlkohol, "north")
  , (PalarniaSmietnik, AlejkaJedzenie, "west")
  , (PalarniaSmietnik, AlejkaNapoje, "east")
  , (PalarniaSmietnik, Wydzial, "south")
  , (Pietro16, PiwnicaPiwnicy, "north")
  , (Wietnam, PiwnicaPiwnicy, "west")
  , (SalaWykladowa, Korytarz1Pietro, "south")
  , (LaboratoriumKomputerowe, Piwnica, "west")
  , (Piwnica, PiwnicaPiwnicy, "south")
  , (Piwnica, Korytarz1Pietro, "north")
  , (PiwnicaPiwnicy, Piwnica, "north")
  , (PiwnicaPiwnicy, Pietro16, "south")
  , (PiwnicaPiwnicy, Wietnam, "east")
  , (PiwnicaPiwnicy, DamskiKibel, "west")
  , (LaboratoriumSieciowe, MeskiKibel, "east")
  , (MeskiKibel, LaboratoriumSieciowe, "west")
  , (MeskiKibel, Korytarz1Pietro, "north")
  , (Korytarz1Pietro, MeskiKibel, "north")
  , (Korytarz1Pietro, Piwnica, "west")
  , (Korytarz1Pietro, SalaWykladowa, "east")
  , (Korytarz1Pietro, Domowka, "south")
  , (EpickaLazienka, Parkiet, "east")
  , (Parkiet, EpickaLazienka, "north")
  , (Parkiet, DrzwiWejsciowe, "south")
  ]

describeLocationName :: Location -> String
describeLocationName = \case
  PokojMarka -> "Pokój Marka"
  PokojBabci -> "Pokój babci"
  Targowek -> "Targówek"
  Jorskiego -> "Ulica Jórskiego"
  Radzyminska -> "Ulica Radzymińska"
  Blokowa -> "Ulica Blokowa"
  Ogrodnicza -> "Ulica Ogrodnicza"
  Makro -> "Sklep Makro"
  AlejkaAlkohol -> "Alejka z alkoholami"
  AlejkaJedzenie -> "Alejka z jedzeniem"
  AlejkaNapoje -> "Alejka z napojami"
  PalarniaSmietnik -> "Palarnia przy śmietniku"
  Wydzial -> "Wydział"
  Pietro16 -> "16 piętro"
  Piwnica -> "Piwnica"
  PiwnicaPiwnicy -> "Piwnica piwnicy"
  SalaWykladowa -> "Sala wykładowa"
  Korytarz1Pietro -> "Korytarz na pierwszym piętrze"
  MeskiKibel -> "Męska toaleta"
  DamskiKibel -> "Damska toaleta"
  Wietnam -> "Wietnam"
  LaboratoriumKomputerowe -> "Laboratorium komputerowe"
  LaboratoriumSieciowe -> "Laboratorium sieciowe"
  Domowka -> "Domówka"
  EpickaLazienka -> "Epicka łazienka"
  Parkiet -> "Parkiet"
  DrzwiWejsciowe -> "Drzwi wejściowe"


type NPCMap = Map.Map String NPC
type ItemMap = Map.Map String PositionedItem

npcMap :: NPCMap
npcMap = Map.fromList [(npcName npc, npc) | npc <- allNPCs]

itemMap :: ItemMap
itemMap = Map.fromList [(itemName item, item) | item <- allPositionedItems]
