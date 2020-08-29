module FoldablesAndFriends where

-- Foldables and friends
--
-- Three common pattern for processing values in data
-- structures
-- + *monoids*, generalise values combination 
-- + *foldables*, generalise folding
-- + *traversables*, generalise mapping

-- ## Monoids
--
-- In mathematics, a *monoid* is a set together with
-- a binary operator and an identity element for the
-- operator
--
-- E.g: the set of integers with the operator `+` and
-- the identity value of `0` forms a monoid

class Monoid' a where
    mempty' :: a -- identity
    mappend' :: a -> a -> a

    mconcat' :: [a] -> a
    mconcat' = foldr mappend' mempty'

-- The two primitives in `Monoid` class are required
-- to sastify the following identity and associativity
-- laws:
--
-- ```haskell
-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- x `mappend` (y `mappend` z) = (x `mappend` y) `mappend` z
-- ```
--
-- `mappend` is ensured by the monoid laws that the
-- order of evaluation does not matter. So parentheses
-- are not needed.
--
-- ```haskell
-- x `mappend` (y `mappend` (z `mappend` mempty))
--
-- -- equivalent
--
-- x `mappend` y `mappend` z
-- ```

instance Monoid' [a] where
  -- mempty' :: List a
  mempty'  = []

  -- mappend' :: [a] -> [a] -> [a]
  mappend' = (++)


exampleMonoidList01 = mappend' [1, 2, 3] []
exampleMonoidList02 = mappend' [] [1, 2, 3]
exampleMonoidList03 = mappend' [] []
exampleMonoidList04 = mappend' [1, 2, 3] [4, 5, 6]


-- `Maybe a` can be made into a monoid if the
-- paramter `a` is also a monoid.
instance Monoid' a => Monoid' (Maybe a) where
  -- mempty' :: Maybe a
  mempty' = Nothing

  -- takes a positive route
  -- mappend' :: Maybe a -> Maybe a -> Maybe a
  (Just x) `mappend'` (Just y) = Just (x `mappend'` y)
  Nothing  `mappend'` my       = my
  mx       `mappend'` Nothing  = mx

exampleMonoidMaybe01 = mappend' (Just [1, 2, 3]) (Just [1, 2])
exampleMonoidMaybe02 = mappend' Nothing (Just [1, 2])
exampleMonoidMaybe03 = mappend' (Just [1, 2, 3]) Nothing

-- integers form a monoid under multiplication
-- with the identity = 1

newtype Mul a = Mul a
    deriving (Eq, Ord, Show, Read)

instance Num a => Monoid' (Mul a) where
  -- mempty' :: Mul a 
  mempty' = Mul 1

  -- mappend' :: Mul a -> Mul a -> Mul a
  (Mul x) `mappend'` (Mul y) = Mul (x * y)


exampleMonoidIntMul01 = mappend' (Mul 1) (Mul 5)
exampleMonoidIntMul02 = mappend' (Mul 5) (Mul 1)
exampleMonoidIntMul03 = mappend' (Mul 17) (Mul 19)


-- integers form a monoid under addition
-- with the identity = 0

newtype Sum a = Sum a
    deriving (Eq, Ord, Show, Read)

instance Num a => Monoid' (Sum a) where
  -- mempty' :: Prod a 
  mempty' = Sum 0

  -- mappend' :: Prod a -> Prod a -> Prod a
  (Sum x) `mappend'` (Sum y) = Sum (x + y)

exampleMonoidIntSum01 = mappend' (Sum 0) (Sum 5)
exampleMonoidIntSum02 = mappend' (Sum 5) (Sum 0)
exampleMonoidIntSum03 = mappend' (Sum 987654321) (Sum 123456789)


-- ## Foldable
--
-- One of the primary applications of monoids in
-- Haskell is to combine all the values in a data
-- structure to give a single value.

foldList :: Monoid a => [a] -> a
foldList []       = mempty
foldList (x : xs) = x `mappend` foldList xs

-- `fold` behaves the same way as `mconcat` from the
-- `Monoid` class, but it is defined using explicit
-- recursion rather than using `foldr`

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

foldTree :: Monoid a => Tree a -> a
foldTree (Leaf x  ) = x
foldTree (Node l r) = foldTree l `mappend` foldTree r

-- The idea of folding up values in data structure
-- using monoid can be abstracted to a range of
-- parameterised types.
--
-- + `fold` combines the elements using monoid 
-- primitives to get a single value of `a`.
--
-- + `foldMap` applies a function to each element
-- in the structure prior to combining the resulting
-- values using the monoid primitives for type `b`.
--
-- + `foldr` and `foldl` generalise the higher-order
-- functions for lists to other data structures.
--
-- ```haskell
-- class Foldable t where
--     fold :: Monoid a => t a -> a
--     foldMap :: Monoid b => (a -> b) -> t a -> b
--     foldr :: (a -> b -> b) -> b -> t a -> b
--     foldl :: (a -> b -> a) -> a -> t b -> a
-- ```

instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
  fold (Leaf x  ) = x
  fold (Node l r) = fold l `mappend` fold r
