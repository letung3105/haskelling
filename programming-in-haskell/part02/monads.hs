module Monads where

-- Consider the following type of expressions that
-- are built up from integer values using a division
-- operator

data Expr = Val Int | Div Expr Expr


-- Such expressions can be evaluated as follows,
-- however, this function does not take account if
-- the posibility of division by zero, an will
-- produce an error

exampleUnsafe = unsafeEval (Div (Val 1) (Val 0))

unsafeEval :: Expr -> Int
unsafeEval (Val n  ) = n
unsafeEval (Div x y) = div (unsafeEval x) (unsafeEval y)

-- we can use the `Maybe a` type to define a safe
-- version of division that returns `Nothing`
-- when the second argument is zero

exampleSafe = eval (Div (Val 1) (Val 0))

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (div x y)

eval :: Expr -> Maybe Int
eval (Val n  ) = Just n
eval (Div x y) = case eval x of
  Nothing -> Nothing
  Just x' -> case eval y of
    Nothing -> Nothing
    Just y' -> safediv x' y'

-- the definition of `eval` is now rather verbose,
-- we can use the fact that `Maybe` is applicative
-- and redefine eval in applicative style
--
-- ```haskell
-- eval' :: Expr -> Maybe Int
-- eval' (Val n  ) = pure n
-- eval' (Div x y) = safediv <$> eval' x <*> eval' y
-- ```
--
-- But this definition is not type correct `safediv`
-- has type `Int -> Int -> Maybe Int` but the context
-- requires `Int -> Int -> Int`
-- 
-- The function `eval` does not fit the pattern of
-- effecful programming captured by applicative
-- functors
-- + Applicative style restricts us to applying pure
-- functions to effectful arguments
-- => `eval` does not fit because `safediv` itself
-- is not a pure function
--
-- How can we rewrite `eval :: Expr -> Maybe Int`
-- in a simpler manner?
--
-- It is observed that there's a common pattern that
-- occurs twice in its definition, namely performing
-- a case analysis on a `Maybe value
-- 1. Mapping `Nothing` to itself
-- 2. Mapping `Just x` to some result depending on `x`
--
-- ```haskell
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- mx >>= f = case mx of
--              Nothing -> Nothing
--              Just x -> f x
-- ```
--
-- `>>=` takes an argument of type `a` that may fail
-- and a function of type `a -> b` whose result may
-- fail, and returns a result of type `b` that may
-- fail. The failure is propagated, otherwise the
-- resulting value is processed o
--
--
exampleSafe' = eval' (Div (Val 1) (Val 0))

eval' :: Expr -> Maybe Int
eval' (Val n  ) = Just n
eval' (Div x y) = eval' x >>= \x' -> eval' y >>= \y' -> safediv x' y'

-- expression that is built using `>>=` has
-- the following structure:
-- 
-- ```haskell
-- m1 >>= \x1 ->
-- m2 >>= \x2 ->
-- .
-- .
-- .
-- mn >>= \xn -> 
-- f x1 x2 ... xn
-- ```
-- 
-- This expression only succeeds if every component
-- `mi` in the sequence secceeds.
--
-- Haskell provides a special notation for expressions
-- of the above form, allowing them to be written in
-- a simpler manner.
--
-- ```haskell
-- do x1 <- m1
--  x2 <- m2
--  .
--  .
--  .
--  xn <- mn
--  f x1 x2 ... xn
-- ```
--
exampleSafe'' = eval'' (Div (Val 1) (Val 0))

eval'' :: Expr -> Maybe Int
eval'' (Val n  ) = Just n
eval'' (Div x y) = do
  x' <- eval'' x
  y' <- eval'' y
  safediv x' y'

-- More generally, the `do` notation is not specific
-- to the types `IO` and `Maybe`, but can be used with
-- any applicative type that forms a *monad*.
-- In Haskell, the concept of a monad is captured by
-- the following built-in declaration
--
-- ```haskell
-- class Applicative m => Monad m where
--  return :: a -> m a
--  (>>=) :: m a -> (a -> m b) -> m b
--
--  return = pure
--  ```

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

ex = pairs [1, 2, 3] [4, 5, 6]

