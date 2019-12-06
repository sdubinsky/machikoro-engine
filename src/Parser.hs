module Parser where
import Board
import Player
--GameState is my object
--This becomes a monad when I add the history to it?

-- commands that just print to the screen.  Includes error, for simplicity
data ShowCommand = ShowHands | ShowField | ShowHistory | ShowBoard | IllegalCommand | InvalidCommand deriving (Show, Eq)
type ParseResult = Either String Board
parseCommand :: String -> Board -> ParseResult
parseUpdateInput :: [String] -> Board -> ParseResult
parseMoveCardToPlayer :: [String] -> Board -> ParseResult
parseAddPlayer :: [String] -> Board -> ParseResult
parseActivate :: [String] -> Board -> ParseResult
matchByName :: (a -> String) -> String -> [a] -> Either String a
updateList :: (Eq a) => a -> [a] -> [a]

 -- processRoll: take a card and two players.  For the second player,
  -- take their hand.  Check every card.  If it activates, add to their score.
  -- if it's a red card, subtract from the first player's total.
  -- return new players with the updated totals

parseCommand input board =
  let inputs = words input in
    case head inputs of 
      "field"   -> Left $ show $ field board
      "players" -> Left $ show $ players board
      "hands"   -> Left $ show [show $ hand x | x <- players board]
      "history" -> Left $ show $ turnHistory board
      "board"   -> Left $ show board
      "help"    -> Left "Available commands:\n  field\n  players\n  hands\n  board\n  move\n  add\n  activate\n  help"
      _         -> parseUpdateInput inputs board

parseUpdateInput [] _ = Left $ "Invalid input"
parseUpdateInput [x] _ = Left $ "Incorrect command: " ++ (show x)
parseUpdateInput (command:params) board =
  case command of
        "move" -> parseMoveCardToPlayer params board
        "add" -> parseAddPlayer params board
        "activate" -> parseActivate params board
        _ -> Left $ "incorrect command: " ++ command ++ " " ++ (show params)

parseMoveCardToPlayer [] _ = Left "Error: no card specified"
parseMoveCardToPlayer [x] _ = Left $ "Incorrect command: " ++ (show x)
parseMoveCardToPlayer (cName: pName:_) board =
  do
    card <- findFieldCardByName cName (field board)
    player <- matchByName playerName pName (players board)
    let cost = cardCost card in
      if cost < (cash player)
      then
        let newPlayer = player { cash = (cash player) - cost} in
            Right $ moveCardToPlayer card newPlayer board { players = updatePlayerList newPlayer $ players board}
      else
        Left "Error: Insufficient funds"

parseAddPlayer [] _ = Left "Error: No player name specified"
parseAddPlayer (name:_) board =
  let samePlayers = filter (\x -> name == (playerName x)) $ players board in
    if null samePlayers
    then Right $ addPlayer name board
    else Left "Error: Player already exists"

parseActivate [] _ = Left "Not enough arguments"
parseActivate [_] _ = Left "Not enough arguments"
parseActivate (iName:pName:_) board =
  do
    player <- matchByName playerName pName $ players board
    imp <- matchByName impName iName $ improvements player
    if impActive imp
      then Left "Card already active"
      else Right $ board {players = updateList player { improvements = updateList imp $ improvements player } $ players board}

matchByName f name list =
  let results = filter (\x -> name == f x) list
  in
    if null results
    then Left $ "Error: " ++ name ++ " not found"
    else Right $ head results

updateList _ [] = []
updateList thing (x:xs)
  | thing == x = thing:xs
  | otherwise = x:updateList thing xs
