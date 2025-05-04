module Main where

import Board
import System.IO

symbolForPlayer :: Int -> Char
symbolForPlayer 0 = '.'
symbolForPlayer 1 = 'X'
symbolForPlayer 2 = 'O'
symbolForPlayer _ = '?'

promptColumn :: Grid -> Int -> IO Int
promptColumn grid currentPlayer = do
  putStrLn $ "Player " ++ [symbolForPlayer currentPlayer] ++ ", choose column (1-" ++ show (totalColumns grid) ++ "):"
  input <- getLine
  let parsed = reads input :: [(Int, String)]
  if null parsed
    then retry
    else let (col, _) = head parsed in
      if col >= 1 && col <= totalColumns grid && columnAvailable grid col
        then return col
        else retry
  where
    retry = do
      putStrLn "Invalid column. Please try again."
      promptColumn grid currentPlayer

gameLoop :: Grid -> Int -> IO ()
gameLoop grid currentPlayer = do
  putStrLn $ gridToString symbolForPlayer grid
  column <- promptColumn grid currentPlayer
  let updatedGrid = insertDisc grid column currentPlayer
  if hasWinner updatedGrid currentPlayer
    then do
      putStrLn $ gridToString symbolForPlayer updatedGrid
      putStrLn $ "Player " ++ [symbolForPlayer currentPlayer] ++ " wins!"
    else if gridIsFull updatedGrid
      then do
        putStrLn $ gridToString symbolForPlayer updatedGrid
        putStrLn "The game is a draw!"
      else gameLoop updatedGrid (if currentPlayer == playerOne then playerTwo else playerOne)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Connect Four (Alternate Version)!"
  let initialGrid = createGrid 7 6
  gameLoop initialGrid playerOne
