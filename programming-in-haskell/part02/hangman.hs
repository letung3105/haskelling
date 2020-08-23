module HangMan where

import           System.IO

-- handman game
-- first player: get string to be guessed
-- second player: get the secrect string
hangman :: IO ()
hangman = do
  putStrLn "Think of a word:"
  word <- sgetLine
  putStrLn "Try to guess it:"
  play word

-- read character by character, and replace the character that has already been read
-- with the character '-'
sgetLine :: IO String
sgetLine = do
  x <- getCh
  if x == '\n'
    then do
      putChar x
      return []
    else do
      putChar '-'
      xs <- sgetLine
      return (x : xs)


-- getCh read a character without echoing it back to the screen
getCh :: IO Char
getCh = do
  hSetEcho stdin False -- turn off echo
  x <- getChar
  hSetEcho stdin True -- turn n echo
  return x

-- read the guessed string
-- terminate if the guess is correct
-- otherwise, print the intersection of the guessed string and the secrect string
play :: String -> IO ()
play word = do
  putStr "?"
  guess <- getLine
  if guess == word
    then putStrLn "You Got It!!!"
    else do
      putStrLn (match word guess)
      play word

match :: String -> String -> String
match xs ys = [ if x `elem` ys then x else '-' | x <- xs ]
