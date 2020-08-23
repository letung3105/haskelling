module StrLen where

-- read character by character until encounter '\n'
getln :: IO String
getln = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- getln
      return (x : xs)

-- print out a string
putstr :: String -> IO ()
putstr []       = return ()
putstr (x : xs) = do
  putChar x
  putstr xs

-- print out a string appended by a newline character '\n'
putstrln :: String -> IO ()
putstrln xs = do
  putStr xs
  putChar '\n'

-- prompt for a string, then output its length
strln :: IO ()
strln = do
  putstr "Enter a string: "
  xs <- getln
  putstr "The string has "
  putstr $ show $ length xs
  putstrln " characters"
