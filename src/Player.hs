module Player where

data Player = Player {playerName :: String, hand :: Hand, cash :: Integer, improvements :: Improvements} deriving Show
instance Eq Player where
  x == y = (playerName x == playerName y)

data Improvement = Improvement {impName :: String,
                   impDescription :: String,
                   impCost :: Integer,
                   impActive :: Bool} deriving Show
instance Eq Improvement where
  x == y = (impName x == impName y)

data Card = Card {cardName :: String,
                  cardCost :: Integer,
                  cardDescription :: String} deriving Show

instance Eq Card where
  x == y = (cardName x == cardName y)

type Hand = [Card] -- the cards the player bought from the field
type Improvements = [Improvement] -- the city improvements you buy to win

initialImprovements :: [Improvement]
initialImprovements =
  [
    Improvement {impName = "City Hall", impDescription = "", impActive = False, impCost = 2},
    Improvement {impName = "Harbor", impDescription = "", impActive = False, impCost = 0},
    Improvement {impName = "Shopping Mall", impDescription = "", impActive = False, impCost = 4},
    Improvement {impName = "Amusement Park", impDescription = "", impActive = False, impCost = 10},
    Improvement {impName = "Radio Tower", impDescription = "", impActive = False, impCost = 16},
    Improvement {impName = "Radio Tower", impDescription = "", impActive = False, impCost = 22},
    Improvement {impName = "Airport", impDescription = "", impActive = False, impCost = 30}
  ]
