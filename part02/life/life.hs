-- The Game of Life
-- ===
-- 
-- The game models a simple evolutionary system based on cells,
-- and it is played on a two-dimensional board. Each square on the board
-- is either empty, or contains a single living cell.
-- 
-- ```
-- .....
-- ...x.
-- .x.x.
-- ..xx.
-- .....
-- ```
-- 
-- Each internal square on the board has eight immediate neighbours.
-- For uniformity, each external square on the board is also viewed
-- as having eight neighbours, by assuming that the board wraps around
-- from **top-to-bottom** and **from left-to-right**.
-- 
-- Given an initial configuration of the board, the next *generation* of the board
-- is given by simultaneously applying the following rules to all squares:
-- 1. a living cell survives if it has precisely two or three neighbouring squares
-- that contain living cells, and
-- 2. an empty square gives birth to a living cell if it has precisely
-- three neighbours that contain living cells, and remains empty otherwise.
-- 
-- Applying the rules to the board above gives:
-- 
-- ```
-- .....
-- ..x..
-- ...xx
-- ..xx.
-- .....
-- ```
-- 
-- By repeating this procedure with the new board, an infinite sequence of generations
-- can be produces.
-- By carefully design of the initial configuration, many interesting patterns
-- of behaviour can be observed in such sequences.
-- 
-- The above arrangement of cells is called a glider, and over successive generations
-- will move diagonally down the board.
-- Despite its simplicity, the game of life is in fact computationally complete.

module Main where

-- an action to clear the screen.
cls :: IO ()
cls = putStr "\ESC[2J"

-- position of each character on the screen is given by a pair of positive integers,
-- with `(1, 1)` being the top-left corner.
type Pos = (Int, Int)

-- move the cursor to the given position, then print the given string.
writeAt :: Pos -> String -> IO ()
writeAt p xs = do
  goto p
  putStr xs

-- move to cursor to the given position.
goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- width of the game of life board.
width :: Int
width = 10

-- height of the game of life board.
height :: Int
height = 10

-- board is represented by a list of (x, y)-coordinates
-- at which there is a living cell.
type Board = [Pos]

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

-- `sequence_ :: [IO a] -> IO ()` performs a list of actions in sequence,
-- discarding their result values and returning no result.
showCells :: Board -> IO ()
showCells b = sequence_ [ writeAt p "0" | p <- b ]

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

-- get neighbours of a given coordinate, with wrap-around
neighbours :: Pos -> [Pos]
neighbours (x, y) = map
  wrap
  [ (x - 1, y - 1)
  , (x    , y - 1)
  , (x + 1, y - 1)
  , (x - 1, y)
  , (x + 1, y)
  , (x - 1, y + 1)
  , (x    , y + 1)
  , (x + 1, y + 1)
  ]

rmdups :: [Pos] -> [Pos]
rmdups []       = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1, ((y - 1) `mod` height) + 1)

liveNeighbours :: Board -> Pos -> Int
liveNeighbours b = length . filter (isAlive b) . neighbours

evolves :: Board -> [Pos]
evolves b = [ p | p <- b, liveNeighbours b p `elem` [2, 3] ]

births :: Board -> [Pos]
births b =
  [ p
  | p <- rmdups (concatMap neighbours b)
  , isEmpty b p
  , liveNeighbours b p == 3
  ]

nextgen :: Board -> Board
nextgen b = evolves b ++ births b

life :: Board -> IO ()
life b = do
  cls
  showCells b
  wait 500000
  life (nextgen b)


main :: IO ()
main = life glider


wait :: Int -> IO ()
wait n = sequence_ [ return () | _ <- [1 .. n] ]
