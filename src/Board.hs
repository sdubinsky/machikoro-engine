module Board where
import Player
data Board = Board {players :: [Player],
                    deck :: Deck,
                    field :: Field,
                    turnHistory :: TurnHistory }

data FieldCard = FieldCard {fieldCard :: Card,
                           fieldCardCount :: Int} deriving (Show, Eq)
data Turn = Turn {activePlayer :: Player,
                  roll :: DiceRoll,
                  cardBought :: Card
                 } deriving Show

instance Show Board where
  show (Board ps _ f _) = "Players:\n" ++ show ps ++ "\nField:\n" ++ show f

data DiceRoll = DiceRoll Int Int deriving Show

type Deck = [Card] -- the deck we draw from
type Field = [FieldCard] -- the ten faceup cards
type TurnHistory = [Turn] -- all prior turns

initialDeck :: Deck
initialBoard :: Board
dealField :: Board -> Int -> Board

addCardToField :: Card -> Field -> Field
deleteCardFromField :: Card -> Field -> Field
addPlayer :: String -> Board -> Board
findFieldCardByName :: String -> Field -> Either String Card
findPlayerByName :: String -> [Player] -> Either String Player
moveCardToPlayer :: Card -> Player -> Board -> Board
updatePlayerList :: Player -> [Player] -> [Player]

  
moveCardToPlayer card player board =
  let newCard = head $ deck board
      newDeck = tail $ deck board
      newField = addCardToField newCard $ deleteCardFromField card $ field board
      newHand = card : hand player
      newPlayer = player { hand = newHand}
      newPlayers = updatePlayerList newPlayer $ players board
      newBoard = board { deck = newDeck, field = newField, players = newPlayers}
      in newBoard

updatePlayerList _ [] = []
updatePlayerList player (x:xs)
  | player == x = player : xs
  | otherwise = x : updatePlayerList player xs

addCardToField card [] = [FieldCard { fieldCard = card, fieldCardCount = 1}]
addCardToField card (x:xs) 
  | card == fieldCard x = x {fieldCardCount = fieldCardCount x + 1} : xs
  | otherwise = x : addCardToField card xs

deleteCardFromField _ [] = []
deleteCardFromField card (x:xs)
  | card == fieldCard x = if fieldCardCount x > 1
                        then x {fieldCardCount = fieldCardCount x - 1} : xs
                        else xs
  | otherwise = x : deleteCardFromField card xs

addPlayer name board =
  let newPlayer = Player { playerName = name, hand = [], cash = 3, improvements = initialImprovements }
  in
    board { players = newPlayer : (players board) }

dealField baseBoard maxLength =
  if length (field baseBoard) >= maxLength
  then baseBoard
  else
    let card = head $ deck baseBoard
        newDeck = tail $ deck baseBoard
        newField = addCardToField card $ field baseBoard
        newBoard = baseBoard { deck = newDeck, field = newField }
    in dealField newBoard maxLength

findFieldCardByName name [] = Left $ "Error: Could not find card " ++ name
findFieldCardByName name (x:xs)
  | (cardName . fieldCard) x == name = Right $ fieldCard x
  | otherwise = findFieldCardByName name xs

findPlayerByName name [] = Left $ "Error: Could not find player " ++ name
findPlayerByName name (x:xs)
  | playerName x == name = Right x
  | otherwise = findPlayerByName name xs

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
  [Card{cardName = "Card" ++ show x, cardDescription = "carddesc" ++ show x, cardCost = x} | x <- [0..100]]
