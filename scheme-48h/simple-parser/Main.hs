module Main where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )

main :: IO ()
main = do
  s <- getLine
  putStrLn $ readExpr s


-- accepted lisp symbols
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"


-- read in the string and check if it is valid
readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found value"

