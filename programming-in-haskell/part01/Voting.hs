module Voting where


-- One vote per person
-- Most votes win


votes :: [String]
votes = ["red", "blue", "green", "blue", "blue", "red"]


ballots :: [[String]]
ballots =
  [ ["Red", "Green"]
  , ["Blue"]
  , ["Green", "Red", "Blue"]
  , ["Blue", "Green", "Red"]
  , ["Green"]
  ]


sort :: Ord a => [a] -> [a]
sort []       = []
sort (x : xs) = sort smaller ++ [x] ++ sort larger
 where
  smaller = [ x' | x' <- xs, x' <= x ]
  larger  = [ x' | x' <- xs, x' > x ]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)


rmDups :: Eq a => [a] -> [a]
rmDups []       = []
rmDups (x : xs) = x : rmDups (filter (/= x) xs)


result :: Ord a => [a] -> [(Int, a)]
result xs = sort [ (count x xs, x) | x <- rmDups xs ]


-- First pass the post
winner :: Ord a => [a] -> a
winner = snd . last . result


eliminate :: Eq a => a -> [[a]] -> [[a]]
eliminate x = map (filter (/= x))


rmEmpties :: [[a]] -> [[a]]
rmEmpties = filter (not . null)


rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head


-- Alternative vote
winner' :: Ord a => [[a]] -> a
winner' xs = case rank $ rmEmpties xs of
  [c     ] -> c
  (c : cs) -> winner' $ eliminate c xs
