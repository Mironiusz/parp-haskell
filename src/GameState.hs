module GameState where

data Location =
    PokojMarka | PokojBabci | Targowek | Jorskiego | Radzyminska | Blokowa | Ogrodnicza
  | Makro | AlejkaAlkohol | AlejkaJedzenie | AlejkaNapoje | PalarniaSmietnik
  | Wydzial | Pietro16 | Piwnica | PiwnicaPiwnicy | SalaWykladowa | Korytarz1Pietro
  | MeskiKibel | DamskiKibel | Wietnam | LaboratoriumKomputerowe | LaboratoriumSieciowe
  | Domowka | EpickaLazienka | Parkiet | DrzwiWejsciowe
  deriving (Eq, Show, Enum, Bounded, Ord)

data GameStatePhase = Phase0 | Phase1 | Phase2 | Phase3 | Phase4 | Phase5 deriving (Eq, Show, Ord)

data PartyItem = PartyItem { pName :: String, pPrice :: Int, pQuality :: Int } deriving (Eq, Show)
data FightItem = FightItem { fName :: String, fHeal :: Int, fDamage :: Int } deriving (Eq, Show)
data MoneyItem = MoneyItem { mName :: String, mValue :: Int } deriving (Eq, Show)

data NPC = NPC
  { npcName :: String
  , npcLocation :: Location
  } deriving (Eq, Show)

data PositionedItem = PositionedItem
  { itemName :: String
  , itemLocation :: Location
  } deriving (Eq, Show)

data GameState = GameState
  { playerLocation :: Location
  , playerInventory :: [String]
  , partyInventory :: [String]
  , playerMoney :: Int
  , guestList :: [String]
  , currentPhase :: GameStatePhase
  , partyQuality :: Int
  , guestsCount :: Int
  , inFight :: Bool
  , playerHp :: Int
  , enemyHp :: Int
  , moneyItemTaken :: Int
  , npcs :: [NPC]
  , positionedItems :: [PositionedItem]
  }

initialState :: [NPC] -> [PositionedItem] -> GameState
initialState npcsData posItems = GameState
  { playerLocation = PokojMarka
  , playerInventory = []
  , partyInventory = []
  , playerMoney = 110
  , guestList = []
  , currentPhase = Phase0
  , partyQuality = 0
  , guestsCount = 1
  , inFight = False
  , playerHp = 100
  , enemyHp = 100
  , moneyItemTaken = 0
  , npcs = npcsData
  , positionedItems = posItems
  }

-- initialState :: [NPC] -> [PositionedItem] -> GameState
-- initialState npcsData posItems = GameState
--   { playerLocation = PokojMarka
--   , playerInventory = []
--   , partyInventory = []
--   , playerMoney = 100
--   , guestList = []
--   , currentPhase = Phase0
--   , partyQuality = 0
--   , guestsCount = 1
--   , inFight = False
--   , playerHp = 100
--   , enemyHp = 100
--   , moneyItemTaken = 0
--   , npcs = npcsData
--   , positionedItems = posItems
--   }