module Lib where
import Data.List

data Card = Card {cardName :: String,
                  cardDescription :: String} deriving Show
data FieldCard = FieldCard {fieldCard :: Card,
                           fieldCardCount :: Int} deriving (Show, Eq)
instance Eq Card where
  x == y = (cardName x == cardName y)

data DiceRoll = DiceRoll Int Int deriving Show
data Improvement = Improvement {impName :: String,
                   impDescription :: String,
                   impActive :: Bool} deriving Show

data Player = Player {playerName :: String, hand :: Hand, cash :: Integer, improvements :: Improvements} deriving Show
instance Eq Player where
  x == y = (playerName x == playerName y)

data Board = Board {players :: [Player],
                    deck :: Deck,
                    field :: Field,
                    rollHistory :: TurnHistory } deriving Show

data Turn = Turn {activePlayer :: Player,
                  roll :: DiceRoll,
                  cardBought :: Card
                 } deriving Show

type Deck = [Card] -- the deck we draw from
type Field = [FieldCard] -- the ten faceup cards
type Hand = [Card] -- the cards the player bought from the field
type Improvements = [Card] -- the city improvements you buy to win
type TurnHistory = [Turn] -- all prior turns

moveCardToPlayer :: Card -> Player -> Board -> Board
updatePlayerList :: Player -> [Player] -> [Player]
addCardToField :: Card -> Field -> Field

moveCardToPlayer card player board =
  let newCard = head $ deck board
      newDeck = tail $ deck board
      newField = addCardToField newCard $ deleteCardFromField card $ field board
      newHand = card : hand player
      newPlayer = player { hand = newHand}
      newPlayers = updatePlayerList newPlayer $ players board
      newBoard = board { deck = newDeck, field = newField}
      in newBoard

updatePlayerList _ [] = []
deleteFromPlayerList player (x:xs)
  | player == x = player : xs
  | otherwise = x : (updatePlayerList player $ xs)

addCardToField card [] = [FieldCard { fieldCard = card, fieldCardCount = 1}]
addCardToField card (x:xs) 
  | card == fieldCard x = FieldCard { fieldCard = fieldCard x, fieldCardCount = fieldCardCount x + 1} : xs
  | otherwise = x : addCardToField card xs

deleteCardFromField _ [] = []
deleteCardFromField card (x:xs)
  | card == fieldCard x = if fieldCardCount x > 1
                        then x {fieldCardCount = fieldCardCount x - 1} : xs
                        else xs
  | otherwise = x : deleteCardFromField card xs

