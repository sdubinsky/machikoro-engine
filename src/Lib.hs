module Lib
    (
    ) where
import Data.List

data Card = Card {cardName :: String,
                 description :: String} deriving Show
instance Eq Card where
  x == y = (cardName x == cardName y)

data DiceRoll = DiceRoll Int Int deriving Show

type Deck = [Card]
type Field = [Card]
type Hand = [Card]
type Improvements = [Card]
type RollHistory = [DiceRoll]


data Player = Player {playerName :: String, hand :: Hand, cash :: Integer, improvements :: Improvements} deriving Show
instance Eq Player where
  x == y = (playerName x == playerName y)

data Board = Board {players :: [Player], deck :: Deck, field :: Field, rollHistory :: RollHistory } deriving Show

moveCardToPlayer :: Card -> Player -> Board -> Board
updatePlayerList :: Player -> [Player] -> [Player]


moveCardToPlayer card player board =
  let newCard = head $ deck board
      newDeck = tail $ deck board
      newField = newCard : (delete card $ field board)
      newHand = card : hand player
      newPlayer = player { hand = newHand}
      newPlayers = updatePlayerList newPlayer $ players board
      newBoard = board { deck = newDeck, field = newField}
      in newBoard

updatePlayerList player players
  | players == [] = []
  | player == head players = player : tail players
  | otherwise = head players : (updatePlayerList player $ tail players)
