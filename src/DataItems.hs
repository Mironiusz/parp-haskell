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
  [ FightItem "szybkie_okularki" 0 55
  , FightItem "koszulka_z_amppz" 0 55
  , FightItem "szlugi" 10 1
  , FightItem "puszka_kuflowe_mocne" 0 5
  , FightItem "metalowy_pręt" 0 25
  , FightItem "cegła" 0 15
  , FightItem "klucz_francuski" 0 8
  , FightItem "kastet" 0 5
  , FightItem "kebab" 25 0
  , FightItem "śmieci" 0 1
  , FightItem "baseball" 0 10
  , FightItem "tulipan_po_królewskim" 0 5
  , FightItem "perła_export" 10 0
  , FightItem "iqos" 5 0
  , FightItem "butelka_metanolu" 40 0
  , FightItem "strzykawka" 0 15
  , FightItem "gazeta" 0 0
  , FightItem "małpka_z_żabki" 20 0
  , FightItem "pusta_puszka" 0 0
  , FightItem "szalik_legii" 0 10
  , FightItem "pusta_butelka" 0 0
  , FightItem "żołądkowa_gorzka" 10 5
  , FightItem "słoik" 0 0
  , FightItem "duży_czarny_kogut" 20 15
  , FightItem "flaga_lecha" 0 0
  , FightItem "gaz_pieprzowy" 0 20
  , FightItem "opaska_do_włosow" 0 0
  , FightItem "czapka_wpierdolka" 0 15
  , FightItem "białko" 5 0
  , FightItem "strzykawka_ze_sterydami" 0 25
  , FightItem "zeszyt_od_matmy" 0 0
  , FightItem "karta_wzorów_na_analizę" 0 15
  , FightItem "algorytm_genetyczny" 0 0
  , FightItem "sieć_neuronowa" 0 30
  , FightItem "puszka_piwa" 3 0
  , FightItem "blue_curacao" 15 10
  , FightItem "puste_piwo_kraftowe" 0 0
  , FightItem "tulipan_po_żubrówce" 0 10
  , FightItem "pusta_strzykawka" 0 0
  , FightItem "pełna_strzykawka" 0 20
  , FightItem "zelda_na_nintendo_switch" 0 0
  , FightItem "rude_włosy" 0 10
  , FightItem "plastikowa_butelka_po_kustoszu" 5 1
  , FightItem "karty_wzorow_na_amppzty" 0 5
  , FightItem "sajgonki_na_fakturę" 20 15
  , FightItem "mleko_od_starego" 0 0
  , FightItem "jabol_w_puszce" 10 5
  , FightItem "szachy" 0 0
  , FightItem "baniak_jabola" 15 10
  , FightItem "kamień" 0 15
  , FightItem "projekt_zespołowy_1" 0 0
  , FightItem "projekt_pap" 0 15
  , FightItem "ptysie" 0 5
  , FightItem "projekt_risc_v_na_arko" 0 30
  , FightItem "surprise" 1 8
  , FightItem "menelskie_okularki" 10 0
  , FightItem "doritos" 5 0
  ]

allPartyItems :: [PartyItem]
allPartyItems =
  [ PartyItem "żubrówka" 30 1
  , PartyItem "duch_puszczy" 20 2
  , PartyItem "jager" 50 25
  , PartyItem "harnold" 3 1
  , PartyItem "komandos" 5 2
  , PartyItem "orzechówka" 25 15
  , PartyItem "smerf" 10 5
  , PartyItem "pepperoni" 30 6
  , PartyItem "hawajska" 30 1
  , PartyItem "laysy_solone" 10 5
  , PartyItem "laysy_paprykowe" 10 6
  , PartyItem "doritosy_serowe" 20 8
  , PartyItem "maczugi_keczupowe" 15 4
  , PartyItem "pepsi" 20 1
  , PartyItem "cola" 20 2
  , PartyItem "redbull" 7 3
  , PartyItem "tiger" 6 2
  , PartyItem "kong_strong" 6 1
  , PartyItem "vizir" 30 6
  , PartyItem "persil" 30 5
  , PartyItem "perwool" 30 8
  , PartyItem "śmieszne_ciasteczka" 50 10
  , PartyItem "kosmiczny_papierek" 50 40
  ]

allMoneyItems :: [MoneyItem]
allMoneyItems =
  [ MoneyItem "skarpetka" 50
  , MoneyItem "lupa" 10
  , MoneyItem "stare_zdjęcie" 100
  , MoneyItem "szczęka" 100
  , MoneyItem "tabletki" 100
  , MoneyItem "telefon" 3
  ]

allPrezenty :: [(String,String,String,String)]
allPrezenty =
 [("rafałek","komandos","pusta_puszka","szalik_legii"),
  ("kubuś","jager","pusta_butelka","żołądkowa_gorzka"),
  ("maciuś","redbull","słoik","duży_czarny_kogut"),
  ("krzysiu","harnold","flaga_lecha","gaz_pieprzowy"),
  ("tomek","smerf","opaska_do_włosow","czapka_wpierdolka"),
  ("mikołaj","perwool","białko","strzykawka_ze_sterydami"),
  ("martynka","hawajska","zeszyt_od_matmy","karta_wzorów_na_analizę"),
  ("olek","duch_puszczy","algorytm_genetyczny","sieć_neuronowa"),
  ("jureczek","żubrówka","puszka_piwa","blue_curacao"),
  ("bartek","kong_strong","puste_piwo_kraftowe","tulipan_po_żubrówce"),
  ("wojtek","kosmiczny_papierek","pusta_strzykawka","pełna_strzykawka"),
  ("kopytek","laysy_paprykowe","zelda_na_nintendo_switch","rude_włosy"),
  ("jasiulek","harnold","pusta_puszka","plastikowa_butelka_po_kustoszu"),
  ("karol_z_wietnamu","pepperoni","karty_wzorow_na_amppzty","sajgonki_na_fakturę"),
  ("łukasz_2","persil","mleko_od_starego","jabol_w_puszce"),
  ("mati","śmieszne_ciasteczka","szachy","baniak_jabola"),
  ("piotrek","maczugi_keczupowe","kamień","baseball"),
  ("wiktor","doritosy_serowe","projekt_zespołowy_1","projekt_pap"),
  ("dziewczyny","orzechówka","ptysie","projekt_risc_v_na_arko")
 ]

allNPCs :: [NPC]
allNPCs =
  [ NPC "babcia" PokojBabci
  , NPC "krzemarz" Jorskiego
  , NPC "jacuś" Jorskiego
  , NPC "sobix" Jorskiego
  , NPC "trąba" Jorskiego
  , NPC "królik" Jorskiego
  , NPC "stróżyk" Jorskiego
  , NPC "wituś" Jorskiego
  , NPC "kożuch" Radzyminska
  , NPC "świeczka" Radzyminska
  , NPC "król_julian" Radzyminska
  , NPC "śliwka" Radzyminska
  , NPC "duży_eryk" Radzyminska
  , NPC "biały_crook" Blokowa
  , NPC "chudy" Blokowa
  , NPC "jeziorak" Blokowa
  , NPC "niespodzianka" Blokowa
  , NPC "charkacz" Ogrodnicza
  , NPC "mariusz" Ogrodnicza
  , NPC "grabie" Ogrodnicza
  , NPC "gracjan" Ogrodnicza
  , NPC "karol_z_wietnamu" Wietnam
  , NPC "martynka" SalaWykladowa
  , NPC "kopytek" SalaWykladowa
  , NPC "piotrek" Piwnica
  , NPC "bartek" Piwnica
  , NPC "jureczek" PiwnicaPiwnicy
  , NPC "tomek" PiwnicaPiwnicy
  , NPC "wojtek" PiwnicaPiwnicy
  , NPC "maciuś" LaboratoriumKomputerowe
  , NPC "olek" LaboratoriumKomputerowe
  , NPC "kubuś" LaboratoriumKomputerowe
  , NPC "rafałek" LaboratoriumSieciowe
  , NPC "łukasz_2" LaboratoriumSieciowe
  , NPC "krzysiu" MeskiKibel
  , NPC "mikołaj" MeskiKibel
  , NPC "jasiulek" MeskiKibel
  , NPC "mati" Korytarz1Pietro
  , NPC "wiktor" Korytarz1Pietro
  , NPC "adrian" EpickaLazienka
  ]

allPositionedItems :: [PositionedItem]
allPositionedItems =
  [ PositionedItem "szybkie_okularki" PokojMarka
  , PositionedItem "koszulka_z_amppz" PokojMarka

  , PositionedItem "skarpetka" PokojBabci
  , PositionedItem "lupa" PokojBabci
  , PositionedItem "stare_zdjęcie" PokojBabci
  , PositionedItem "szczęka" PokojBabci
  , PositionedItem "tabletki" PokojBabci
  , PositionedItem "telefon" PokojBabci

  , PositionedItem "szlugi" Jorskiego
  , PositionedItem "puszka_kuflowe_mocne" Radzyminska
  , PositionedItem "metalowy_pręt" Blokowa
  , PositionedItem "cegła" Ogrodnicza
  , PositionedItem "klucz_francuski" Jorskiego
  , PositionedItem "kastet" Radzyminska
  , PositionedItem "kebab" Blokowa
  , PositionedItem "śmieci" Ogrodnicza
  , PositionedItem "baseball" Jorskiego
  , PositionedItem "tulipan_po_królewskim" Radzyminska
  , PositionedItem "perła_export" Blokowa
  , PositionedItem "doritos" Ogrodnicza

  , PositionedItem "redbull" Ogrodnicza
  , PositionedItem "strzykawka" Ogrodnicza
  , PositionedItem "gazeta" Ogrodnicza
  , PositionedItem "małpka_z_żabki" Ogrodnicza

  , PositionedItem "żubrówka" AlejkaAlkohol
  , PositionedItem "duch_puszczy" AlejkaAlkohol
  , PositionedItem "jager" AlejkaAlkohol
  , PositionedItem "harnold" AlejkaAlkohol
  , PositionedItem "komandos" AlejkaAlkohol
  , PositionedItem "orzechówka" AlejkaAlkohol
  , PositionedItem "smerf" AlejkaAlkohol

  , PositionedItem "pepperoni" AlejkaJedzenie
  , PositionedItem "hawajska" AlejkaJedzenie
  , PositionedItem "laysy_solone" AlejkaJedzenie
  , PositionedItem "laysy_paprykowe" AlejkaJedzenie
  , PositionedItem "doritosy_serowe" AlejkaJedzenie
  , PositionedItem "maczugi_keczupowe" AlejkaJedzenie

  , PositionedItem "pepsi" AlejkaNapoje
  , PositionedItem "cola" AlejkaNapoje
  , PositionedItem "redbull" AlejkaNapoje
  , PositionedItem "tiger" AlejkaNapoje
  , PositionedItem "kong_strong" AlejkaNapoje

  , PositionedItem "vizir" PalarniaSmietnik
  , PositionedItem "persil" PalarniaSmietnik
  , PositionedItem "perwool" PalarniaSmietnik
  , PositionedItem "śmieszne_ciasteczka" PalarniaSmietnik
  , PositionedItem "kosmiczny_papierek" PalarniaSmietnik

  , PositionedItem "surprise" EpickaLazienka
  , PositionedItem "menelskie_okularki" Parkiet
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
  , (DamskiKibel, PiwnicaPiwnicy, "east")
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
