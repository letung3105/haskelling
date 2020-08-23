module AlternativeVote where


-- Each person can vote for as many / as few candidates as they wish
-- Deciding on the winner
-- (1) Remove empty ballots
-- (2) Remove candidate with smallest number of 1st choice vote
-- (3) Repeated step until only one candidate remains


ballots :: [[String]]
ballots =
  [ ["Red", "Green"]
  , ["Blue"]
  , ["Green", "Red", "Blue"]
  , ["Blue", "Green", "Red"]
  , ["Green"]
  ]


count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)


rmDups :: Eq a => [a] -> [a]
rmDups []       = []
rmDups (x : xs) = x : rmDups (filter (/= x) xs)


sort :: Ord a => [a] -> [a]
sort []       = []
sort (x : xs) = sort smaller ++ [x] ++ sort larger
 where
  smaller = [ x' | x' <- xs, x' <= x ]
  larger  = [ x' | x' <- xs, x' > x ]


result :: Ord a => [a] -> [(Int, a)]
result xs = sort [ (count x xs, x) | x <- rmDups xs ]


-- votingRound :: Ord a => [[a]] -> [[a]]
-- votingRound xs = eliminate (loser $ getFirst $ rmDups xs) xs


eliminate :: Eq a => a -> [[a]] -> [[a]]
eliminate x = map (filter (/= x))


rmEmpties :: [[a]] -> [[a]]
rmEmpties = filter (not . null)


rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head


winner :: Ord a => [[a]] -> a
winner xs = case rank $ rmEmpties xs of
  [c     ] -> c
  (c : cs) -> winner $ eliminate c xs
