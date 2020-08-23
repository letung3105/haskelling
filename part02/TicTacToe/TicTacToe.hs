-- Unbeatable Tic Tac Toe

module Main where

import           Data.Char
import           Data.List
import           System.IO

type Pos = (Int, Int)

-- an action to clear the screen.
cls :: IO ()
cls = putStr "\ESC[2J"

-- move the cursor to the given position, then print the given string.
writeAt :: Pos -> String -> IO ()
writeAt p xs = do
  goto p
  putStr xs

-- move to cursor to the given position.
goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- allow board size to be changed
size :: Int
size = 3

-- a two-dimensional list representing the board state
type Grid = [[Player]]

-- an element in a grid can be X, O, or B (Blank)
data Player = O | B | X
  deriving (Eq, Ord, Show)

-- switch player
next :: Player -> Player
next O = X
next X = O

-- create an empty grid
empty :: Grid
empty = replicate size $ replicate size B

-- no more blanks
full :: Grid -> Bool
full = notElem B . concat

-- determine who is the next player based on the state of the grid
turn :: Grid -> Player
turn g = if os <= xs then O else X
 where
  os = length (filter (== O) ps)
  xs = length (filter (== X) ps)
  ps = concat g

-- check if the player wins in the given grid
wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ diags)
 where
  line  = all (== p)
  rows  = g
  cols  = transpose g
  diags = [diag g, diag (map reverse g)]

-- return the main diagonal of a grid
diag :: Grid -> [Player]
diag g = [ g !! n !! n | n <- [0 .. size - 1] ]

-- check if the game has finished
won :: Grid -> Bool
won g = wins O g || wins X g

-- show the grid
putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
 where
  beside = foldr1 (zipWith (++))
  bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x []       = []
interleave x [y     ] = [y]
interleave x (y : ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = [ chop size (xs ++ [p] ++ ys) | valid g i ]
  where (xs, B : ys) = splitAt i (concat g)

-- group elements in list by size given by the first argument
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- reading a natural number
getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins!"
  | wins X g = putStrLn "Player X wins!"
  | full g = putStrLn "It's a draw!"
  | otherwise = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        run' g p
      [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

-- Game tree captures all the possible ways
-- in which the game can proceed from the current grid

-- a tree of a given type is a node that comprises a value
-- of that type and a list of subtrees
data Tree a = Node a [Tree a]
  deriving (Show)

-- compute the game tree for all posible moves
gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [ gametree g' (next p) | g' <- moves g p ]

-- compute the subtree for all the next move that can be made
moves :: Grid -> Player -> [Grid]
moves g p | won g     = []
          | full g    = []
          | otherwise = concat [ move g i p | i <- [0 .. (size ^ 2 - 1)] ]

-- prune trees to a particular depth
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _ ) = Node x []
prune n (Node x ts) = Node x [ prune (n - 1) t | t <- ts ]

depth :: Int
depth = 9


-- MINIMAX ALGORITHM
-- The algorithm starts by labelling every node in the tree with a player value
-- in the following manner
--  + Leaves are labelled with the winning player at this point if there is one,
--  and the blank player if otherwise.
--  + Inner nodes are labelled with the *minimum* or *maximum* of the player labels
--  from the child nodes one level down, depending on whose turn it is to move
--  at this point: on player O's turn we take the minimum of the child labels,
--  and on X's turn we take the maximum

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g []) | wins O g  = Node (g, O) []
                    | wins X g  = Node (g, X) []
                    | otherwise = Node (g, B) []
minimax (Node g ts) | turn g == O = Node (g, minimum ps) ts'
                    | turn g == X = Node (g, maximum ps) ts'
 where
  ts' = map minimax ts
  ps  = [ p | Node (_, p) _ <- ts' ]

-- we first build the game tree up to the specified depth,
-- apply the minimax algorithm to label the tree
-- select a grid whose player label is the same as that of the root node
--
-- there is always at least on best move, because we are taking
-- the minimum and maximum value from a non-empty list.
-- 
-- if there are multiple best moves, selects the first of them
bestmove :: Grid -> Player -> Grid
bestmove g p = head [ g' | Node (g', p') _ <- ts, p' == best ]
 where
  tree              = prune depth (gametree g p)
  Node (_, best) ts = minimax tree

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  play empty O

play :: Grid -> Player -> IO ()
play g p = do
  cls
  goto (1, 1)
  putGrid g
  play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn " It's a draw!\n"
  | p == O = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        play' g p
      [g'] -> play g' (next p)
  | p == X = do
    putStrLn "Player X is thinking..."
    (play $! bestmove g p) (next p)
