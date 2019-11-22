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
data UpdateCommand = MoveCardToPlayer
type Deck = [Card] -- the deck we draw from
type Field = [FieldCard] -- the ten faceup cards
type Hand = [Card] -- the cards the player bought from the field
type Improvements = [Card] -- the city improvements you buy to win
type TurnHistory = [Turn] -- all prior turns

parseCommand :: String -> Either ShowCommand UpdateCommand
processShowInput :: ShowCommand -> Board -> String
processUpdateInput :: UpdateCommand -> Board -> Board


initialDeck :: Deck
dealField :: Board -> Int -> Board
moveCardToPlayer :: Card -> Player -> Board -> Board
updatePlayerList :: Player -> [Player] -> [Player]
addCardToField :: Card -> Field -> Field
findCardByName :: String -> [Card] -> Card
 -- processRoll: take a card and two players.  For the second player,
  -- take their hand.  Check every card.  If it activates, add to their score.
  -- if it's a red card, subtract from the first player's total.
  -- return new players with the updated totals

initialBoard =
  let baseBoard = Board {players = [],
                          deck = initialDeck,
                          field = [],
                          turnHistory = []
                        }
      board = dealField baseBoard 10
  in
    board
initialDeck =
  [Card{cardName = "Card" ++ show x, cardDescription = "carddesc" ++ show x} | x <- [0..100]]

dealField baseBoard maxLength =
  if length (field baseBoard) >= maxLength
  then baseBoard
  else
    let card = head $ deck baseBoard
        newDeck = tail $ deck baseBoard
        newField = addCardToField card $ field baseBoard
        newBoard = baseBoard { deck = newDeck, field = newField }
    in dealField newBoard maxLength
  
parseCommand input
  | input == "field" = Left ShowField
  | input == "hands" = Left ShowHands
  | input == "history" = Left ShowHistory
  | input == "buy" = Right MoveCardToPlayer
  | otherwise = Left InvalidCommand

processShowInput option board
  | option == ShowField = show $ field board
  | option == ShowHands = show [show $ hand x | x <- players board]
  | option == ShowHistory = show $ turnHistory board
  | option == InvalidCommand = "Error: Invalid Command"

processUpdateInput option board 
  | option == MoveCardToPlayer = processMoveCardToPlayer board
  | otherwise = do
      putStrLn "Error: Invalid Command"
      board

processMoveCardToPlayer board = do
  putStrLn "Which card do you want to move?"
  cardName <- getLine
  myCard <- findCardByName cardName $ field board
  putStrLn "Which player gets the card?"
  playerName <- getLine
  myPlayer <- findPlayerByName playerName $ players board
  moveCardToPlayer myCard myPlayer board
    
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

findCardByName name (x:xs)
  | cardName x == name = x
  | otherwise = findCardByName name xs

findPlayerByName name (x:xs)
  | playerName x == name = x
  | otherwise = findPlayerByName name xs
