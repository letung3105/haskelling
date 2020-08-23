module Nim where

import           Data.Char

data Player = PlayerA | PlayerB
    deriving (Show)

-- number of star(s) left on each row
type Board = [Int]

-- switch player
next :: Player -> Player
next PlayerA = PlayerB
next PlayerB = PlayerA

-- initial board of 5 rows
initial :: Board
initial = [5, 4, 3, 2, 1]

-- game is finished if every row has zero star
finished :: Board -> Bool
finished = all (== 0)

-- given the board, row, and number of star(s) to remove
-- check if the move is valid
valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

-- print the row number and the same number of stars as the given number
putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

-- 
move :: Board -> Int -> Int -> Board
move board row num = zipWith update [1 ..] board
  where update r n = if r == row then n - num else n

-- print for 5 rows board
putBoard :: Board -> IO ()
putBoard [a, b, c, d, e] = do
  putRow 1 a
  putRow 2 b
  putRow 3 c
  putRow 4 d
  putRow 5 e

newline :: IO ()
newline = do
  putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x
    then return (digitToInt x)
    else do
      putStrLn "ERROR: invalid digit"
      getDigit prompt

play :: Board -> Player -> IO ()
play board player = do
  newline
  putBoard board
  if finished board
    then do
      newline
      putStr (show (next player))
      putStrLn " wins!!!"
    else do
      newline
      print player
      row <- getDigit "Enter a row number: "
      num <- getDigit "Stars to remove: "
      if valid board row num
        then play (move board row num) (next player)
        else do
          newline
          putStrLn "ERROR: Invalid move"
          play board player

nim :: IO ()
nim = play initial PlayerA
