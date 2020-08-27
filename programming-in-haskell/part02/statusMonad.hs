module StateMonad where

-- Write functions that manipulate some from of state
-- that can be changed over time.

type State = Int

-- The most basic form of function on this type is a
-- *state transformer*, abbreviated by `ST`, which
-- takes an input state and produces an ouput state
-- along with a result value, with the type being
-- a parameter of the `ST` type.
--
-- ```haskell
-- type ST = State -> State
-- ```
--
-- State transformers may also wish to take argument
-- values, which can be achieved by exploiting
-- currying `Char -> ST Int` is similar to
-- `Char -> State -> (Int, State)`
--
-- ```haskell
-- type ST a = State -> (a, State)
-- ```
--
-- Given that `ST` is a parameterised type, it is
-- natural to try and make it into a monad so that
-- the do notation can then be used to write stateful
-- programs. For that we have to defined `ST` using
-- the `newtype` mechanism

newtype ST a = S (State -> (a, State))

