module Main where


-- Given a sequence of numbers and a target number,
-- attemp to construct an expression whose value
-- is the target, by combining one or more numbers
-- from the sequence using addition, subtraction,
-- multiplication, division, and parentheses.
-- Each number in the sequence can only be used
-- at most once in the expression, and all the numbers
-- involed, including the intermediate values, must
-- be positive natural numbers. In other words, the use
-- of zero, negative numbers, and proper fractions is
-- not permitted.


e1 = isSolution
  (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)))
  [1, 3, 7, 10, 25, 50]
  765


-- arithmetic operators
data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"


-- a numeric expression is either a value or an application of some operator
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n    ) = show n
  show (App o l r) = bracket l ++ show o ++ bracket r
   where
    bracket (Val n) = show n
    bracket e       = "(" ++ show e ++ ")"


-- check if the resulting value is a non-negative natural number
-- for the given operator and numbers
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = mod x y == 0


-- apply the given operator to the numbers
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = div x y


-- returns the list of values shown in the expression
values :: Expr -> [Int]
values (Val n    ) = [n]
values (App _ l r) = values l ++ values r


-- evaluate the expression, a result is returns if only natural numbers
-- shown up in all steps of the evaluation
eval :: Expr -> Maybe Int
eval (Val n) | n > 0     = Just n
             | otherwise = Nothing
eval (App o l r) = case (eval l, eval r) of
  (Just x , Just y ) -> if valid o x y then Just (apply o x y) else Nothing
  (Nothing, _      ) -> Nothing
  (_      , Nothing) -> Nothing


-- returns all subsequences of a list
subs :: [a] -> [[a]]
subs []       = [[]]
subs (x : xs) = yss ++ map (x :) yss where yss = subs xs


-- returns all possible ways of inserting an element into a list
interleave :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)


-- returns all permutations of a list
perms :: [a] -> [[a]]
perms = foldr (concatMap . interleave) [[]]


-- returns all possible ways of selecting zero or more elements in any order
choices :: [a] -> [[a]]
choices = concatMap perms . subs


isSolution :: Expr -> [Int] -> Int -> Bool
isSolution e ns n = case eval e of
  Just n' -> elem (values e) (choices ns) && n' == n
  Nothing -> False


-- all possible ways of splitting a list into 2 non-empty lists
-- that can be appended to give the original list
split :: [a] -> [([a], [a])]
split []       = []
split [_     ] = []
split (x : xs) = ([x], xs) : [ (x : ls, rs) | (ls, rs) <- split xs ]


-- ******** First pass ******** 
-- a brute force solution to the count down problem:
-- generate all possible expressions over the given list of numbers

-- all posible ways to combine 2 expressions
combine :: Expr -> Expr -> [Expr]
combine l r = [ App o l r | o <- [Add, Sub, Mul, Div] ]

-- all possible expressions whose list of values is precisely the given list
exprs :: [Int] -> [Expr]
-- empty list has no expression
exprs []  = []
-- a single integer forms a single Val
exprs [n] = [Val n]
-- if the list has 2 or more elements, for every possible split of the list,
-- recursively compute the expressions that can be generated from each of
-- the 2 parts. Then combine the 2 expresion using every possible operator,
-- where the expressions is taken from the list of expressions generated from
-- the 2 splits, with one of the expressions taken from the first list and the
-- other taken from the second list.
exprs ns =
  [ e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r ]

-- For every possible picks from the list of number, disregarding the order.
-- For each of the pick compute all the possible expression that can be made
-- from the picks.
-- For each generated expression, check if it is valid
solve :: [Int] -> Int -> [Expr]
solve ns n = [ e | ns' <- choices ns, e <- exprs ns', eval e == Just n ]


-- ******** Second pass ******** 
-- We can observe that the number of successful evaluation is much smaller
-- than the number of expressions that can be generated from the list of
-- numbers since subtraction and division is not always valid on natural
-- numbers
-- Base on this observation, an optimization which combine the generation
-- of expressions with their evaluation can be made simultaneously.
-- Expressions that fail to evaluate are rejected, and are not used to generate
-- further expressions


-- expression that is evaluate, and its resulting value
type Result = (Expr, Int)


combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) =
  [ (App o l r, apply o x y) | o <- [Add, Sub, Mul, Div], valid o x y ]


results :: [Int] -> [Result]
results []  = []
results [n] = [ (Val n, n) | n > 0 ]
results ns =
  [ res
  | (ls, rs) <- split ns
  , l        <- results ls
  , r        <- results rs
  , res      <- combine' l r
  ]


solve' :: [Int] -> Int -> [Expr]
solve' ns n = [ e | ns' <- choices ns, (e, m) <- results ns', m == n ]


main :: IO ()
main = do
  putStrLn ("Number of solutions " ++ show (length sols))
 where
  nslist = [1, 3, 7, 10, 25, 50]
  target = 765
  sols   = solve' nslist target
