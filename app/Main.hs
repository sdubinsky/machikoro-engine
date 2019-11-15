module Main where

import Lib

main :: IO ()
loop :: Board -> IO ()
main =
  loop Lib.initialBoard
  
-- https://stackoverflow.com/questions/55755738/haskell-how-do-you-make-a-loop-that-has-getline-within-it
loop board = do
  input <- getLine
  if input == "end"
    then print "goodbye" -- end
    else
    case parseCommand input of
      Left command -> do
        print $ Lib.processShowInput command board
        loop board
      Right command -> do
        loop $ Lib.processUpdateInput command board
