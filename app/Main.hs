module Main where

import Parser
import Board

main :: IO ()
loop :: Board -> IO ()
main =
  loop initialBoard
  
-- https://stackoverflow.com/questions/55755738/haskell-how-do-you-make-a-loop-that-has-getline-within-it
loop board = do
  putStrLn$ ">"
  input <- getLine
  if input == "end"
    then putStrLn $ "goodbye" -- end
    else
    case parseCommand input board of
      Left str -> do
        putStrLn str
        loop board
      Right newBoard -> do
        loop newBoard
