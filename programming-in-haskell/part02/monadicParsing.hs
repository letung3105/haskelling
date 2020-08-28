module MonadicParsing where

import           Control.Applicative
import           Data.Char


-- `Parse` is similar to a state transformer (`ST`)
newtype Parser a = P (String -> [(a, String)])


-- apply parser to an input string, removes the dummy constructor
parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

-- item fails if input is empty, and succeeds with the first character
-- as the result value
item :: Parser Char
item = P p
 where
  p inp = case inp of
    []       -> []
    (x : xs) -> [(x, xs)]

exampleItem01 = parse item "abc"
exampleItem02 = parse item ""


-- class Functor (f :: * -> *) where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a
--   {-# MINIMAL fmap #-}

-- `fmap` applies a function to the result value of a parser if the parser succeeds
-- and propagates the failure otherwise
--
-- `fmap` applies a function to the result value of a paper if the parser
-- secceeds, and propagates the failure otherwise.
instance Functor Parser where
  fmap g p = P out
   where
    out input = case parse p input of
      []       -> []
      [(v, s)] -> [(g v, s)]


exampleFmap01 = parse (fmap toUpper item) "abc"
exampleFmap02 = parse (fmap toUpper item) ""


-- class Functor f => Applicative (f :: * -> *) where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
--   liftA2 :: (a -> b -> c) -> f a -> f b -> f c
--   (*>) :: f a -> f b -> f b
--   (<*) :: f a -> f b -> f a
--   {-# MINIMAL pure, ((<*>) | liftA2) #-}
--
-- `pure` transform a value into a parser that always succeeds
-- with this value as its result.
--
-- `<*>` applies a parser that returns a function to a parser
-- that returns an argument to give a parser hat returns the
-- result of applying the function to the argument, and only
-- succeeds if all the components succeed 
instance Applicative Parser where
  pure v = P p where p inp = [(v, inp)]

  pg <*> px = P out
   where
    out inp = case parse pg inp of
      []       -> []
      [(g, s)] -> parse (fmap g px) s


-- takes three character, and returns the first and last
-- taken characters as a 2-ary tuple
threeA :: Parser (Char, Char)
threeA = g <$> item <*> item <*> item where g x y z = (x, z)

exampleParseThreeA01 = parse threeA "abcdef"
exampleParseThreeA02 = parse threeA "ab"

-- class Applicative m => Monad (m :: * -> *) where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a
--   {-# MINIMAL (>>=) #-}
--
-- `>>=` fails if the application of `p` to the `inp`,
-- otherwise applies `f` to the result value `v`
-- to give another parser `f v`,
-- which is then applied to the output string out that
-- was produced by the first parser to give the final result.
instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P out
   where
    out inp = case parse p inp of
      []       -> []
      [(v, s)] -> parse (f v) s

threeM :: Parser (Char, Char)
threeM = do
  x <- item
  item
  z <- item
  return (x, z)

exampleParseThreeM01 = parse threeM "abcdef"
exampleParseThreeM02 = parse threeM "ab"


-- `do` notation combines parsers in sequence, with the output string
-- from each parser in the sequence becoming the input string for the next.
--
-- another way to combine parsers is to apply one parser, if it fails
-- apply another to the same input instead.
--
-- ```haskell
-- class Applicative f => Alternative f where
--  empty :: f a
--  (<|>) :: f a -> f a -> f a
-- ```
--
-- + `empty` represents an alternative that has failed
-- + `<|>` is an appropriate choice operator
--
-- They are required to sastify the following identity and associativity laws
--
-- ```haskell
-- empty <|> x     = x
-- x <|> empty     = x
-- x <|> (y <|> z) = (x <|> y) <|> z
-- ```
--
-- ```haskell
-- instance Alternative Maybe where
--  -- empty :: Maybe a
--  empty = Nothing
--
--  -- (<|>) :: Maybe a -> Maybe a -> Maybe a
--  Nothing <|> my = my
--  (Just x) <|> _ = Just x
-- ```

instance Alternative Parser where
  empty = P (const [])

  p <|> q = P out
   where
    out inp = case parse p inp of
      []       -> parse q inp
      [(v, s)] -> [(v, s)]


exampleAlternative01 = parse empty "abc"
exampleAlternative02 = parse (empty <|> item) "abc"
exampleAlternative03 = parse (item <|> empty) "abc"


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = satisfy isDigit

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

letter :: Parser Char
letter = satisfy isAlpha

alphanum :: Parser Char
alphanum = satisfy isAlphaNum

char :: Char -> Parser Char
char x = satisfy (== x)

exampleChar01 = parse (char 'a') "abc"
exampleChar02 = parse (char 'd') "abc"

string :: String -> Parser String
string []       = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

exampleString01 = parse (string "Hello") "Hello, world"
exampleString02 = parse (string "world") "Hello, world"


-- The `Alternative` class provides default definitions
-- for two functions `some p` and `many p`
--
-- ```haskell
-- class Applicative f => Alternative f where empty :: f a
--      (<|>) :: f a -> f a -> f a
--      many :: f a -> f [a]
--      some :: f a -> f [a]
--
--      many x = some x <|> pure []
--      some x = pure (:) <*> x <*> many x
-- ```
--
-- The functions are defined using mutual recursion
-- + `many x` states that x can be applied at least
-- once or not at all
-- + `some x` states that x can be applied once
-- then zero or more times


-- parse identifier whose name starts with a
-- lower-case letter, followed by zero or more
-- alphanumeric characters
ident :: Parser String
ident = do
  x  <- lower
  xs <- many alphanum
  return (x : xs)

exampleIdent01 = parse ident "abd def"


-- parse natural number
nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

exampleNat01 = parse nat "123 def"


-- parse spaces
space :: Parser ()
space = do
  many (satisfy isSpace)
  return ()

exampleSpace01 = parse space "  abc"


-- parse an integer number
int :: Parser Int
int =
  do
      char '-'
      n <- nat
      return (-n)
    <|> nat

exampleInt01 = parse int "-123 def"


-- Most real-life parsers allow spacing to be freely
-- used around the basic token in their input string
-- `1+2` and `1 + 2` both parsed the same way by GHC
--
-- We define a new primitive that ignores any space
-- before and after applying a parser for a token
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v


-- parse an identifier,
-- ignore any spaces around it.
identifier :: Parser String
identifier = token ident


-- parse a natural number,
-- ignore any spaces around it.
natural :: Parser Int
natural = token nat


-- parse an integer,
-- ignore any spaces around it.
integer :: Parser Int
integer = token int


-- parse a matching symbol,
-- ignore any spaces around it.
symbol :: String -> Parser String
symbol xs = token (string xs)


-- parse a list of natural numbers enclose by "["
-- and "]"
-- + ignoring any spaces around it the parentheses
-- + inoring any spaces between the elements in
-- the list.
nats :: Parser [Int]
nats = do
  symbol "["
  n  <- natural
  ns <- many
    (do
      symbol ","
      natural
    )
  symbol "]"
  return (n : ns)

exampleNats01 = parse nats " [1, 2, 3] "
exampleNats02 = parse nats "[1,2,]"


-- Arithmetic expressions

expr :: Parser Int
expr = do
  t <- term
  (do
      symbol "+"
      e <- expr
      return (t + e)
    )
    <|> return t

term :: Parser Int
term = do
  f <- factor
  (do
      symbol "*"
      t <- term
      return (f * t)
    )
    <|> return f

factor :: Parser Int
factor =
  (do
      symbol "("
      e <- expr
      symbol ")"
      return e
    )
    <|> natural


eval :: String -> Int
eval xs = case parse expr xs of
  [(n, [] )] -> n
  [(_, out)] -> error ("Unused input " ++ out)
  []         -> error "Invalid input"

exampleEval01 = eval "2*3+4"
exampleEval02 = eval "2*(3+4)"
exampleEval03 = eval "2*3^4"
exampleEval04 = eval "one plus two"
