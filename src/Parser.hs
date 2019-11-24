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

 -- processRoll: take a card and two players.  For the second player,
  -- take their hand.  Check every card.  If it activates, add to their score.
  -- if it's a red card, subtract from the first player's total.
  -- return new players with the updated totals

parseCommand input board =
  let inputs = words input in
    case head inputs of 
      "field" -> Left $ show $ field board
      "hands" -> Left $ show [show $ hand x | x <- players board]
      "history" -> Left $ show $ turnHistory board
      "board" -> Left $ show board
      _ -> parseUpdateInput inputs board

parseUpdateInput inputs board =
  case head inputs of
        "move" -> parseMoveCardToPlayer (tail inputs) board
        "add" -> parseAddPlayer (tail inputs) board
        _ -> Left $ "incorrect command: " ++ (show inputs)

parseMoveCardToPlayer inputs board =
  do
    card <- findFieldCardByName (head inputs) (field board)
    player <- findPlayerByName (head $ tail inputs) (players board)
    let cost = cardCost card in
      if cost < (cash player)
      then
        let newPlayer = player { cash = cost - (cash player)} in
            Right board { players = updatePlayerList newPlayer $ players board}
      else
        Left "Error: Insufficient funds"

parseAddPlayer inputs board =
  let samePlayers = filter (\x -> (head inputs) == playerName x) $ players board in
    if null samePlayers
    then Right $ addPlayer inputs board
    else Left "Error: Player already exists"
