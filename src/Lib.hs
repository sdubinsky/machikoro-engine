module Lib where
import Data.List

--GameState is my object
--This becomes a monad when I add the history to it?
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
                    turnHistory :: TurnHistory } deriving Show

data Turn = Turn {activePlayer :: Player,
                  roll :: DiceRoll,
                  cardBought :: Card
                 } deriving Show

-- commands that just print to the screen.  Includes error, for simplicity
data ShowCommand = ShowHands | ShowField | ShowHistory | InvalidCommand deriving (Show, Eq)
data UpdateCommand
type Deck = [Card] -- the deck we draw from
type Field = [FieldCard] -- the ten faceup cards
type Hand = [Card] -- the cards the player bought from the field
type Improvements = [Card] -- the city improvements you buy to win
type TurnHistory = [Turn] -- all prior turns

parseCommand :: String -> Either ShowCommand UpdateCommand
processShowInput :: ShowCommand -> Board -> String
processUpdateInput :: UpdateCommand -> Board -> Board


initialDeck :: Deck
--makeField :: Board -> Board
moveCardToPlayer :: Card -> Player -> Board -> Board
updatePlayerList :: Player -> [Player] -> [Player]
addCardToField :: Card -> Field -> Field
 -- processRoll: take a card and two players.  For the second player,
  -- take their hand.  Check every card.  If it activates, add to their score.
  -- if it's a red card, subtract from the first player's total.
  -- return new players with the updated totals

initialBoard = 
  Board {players = [],
                  deck = [],
                  field = [] ,
                  turnHistory = []
                 }
initialDeck =
  [Card{cardName = "Card" ++ show x, cardDescription = "carddesc" ++ show x} | x <- [0..100]]
parseCommand input
  | input == "field" = Left ShowField
  | input == "hands" = Left ShowHands
  | input == "history" = Left ShowHistory
  | otherwise = Left InvalidCommand

processShowInput option board
  | option == ShowField = show $ field board
  | option == ShowHands = show [show $ hand x | x <- players board]
  | option == ShowHistory = show $ turnHistory board
  | option == InvalidCommand = "Error: Invalid Command"

processUpdateInput option board = undefined
    
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
updatePlayerList player (x:xs)
  | player == x = player : xs
  | otherwise = x : updatePlayerList player xs

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

