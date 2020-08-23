module AbstractMachine where


data Expr = Val Int | Add Expr Expr


{-
value :: Expr -> Int
value (Val n  ) = n
-- the order of evaluation, whether `value x` or `value y` is evaluate first,
-- is determined by Haskell, and we have no control over it
value (Add x y) = value x + value y
-}


-- we define an abstract machine for expressions, which specifies
-- the step-by-step process of their evaluation

type Cont = [Op] -- control stack: operations to be performed
data Op = EVAL Expr | ADD Int


eval :: Expr -> Cont -> Int
-- if the expression is an integer, i.e.: it is fully evaluated,
-- we execute the control stack
eval (Val n  ) c = exec c n
-- if the expression is an addition, we evaluate the first expression
-- then push the operation to evaluate second expression to the control stack
eval (Add x y) c = eval x (EVAL y : c)


exec :: Cont -> Int -> Int
-- return if control stack is empty
exec []           n = n
-- if the next OP is EVAL, evaluate the given expression,
-- and pushes ADD to the control stack
exec (EVAL y : c) n = eval y (ADD n : c)
-- if the next operation is ADD, add the values, then continue
-- to execute the next OP
exec (ADD  m : c) n = exec c (n + m)

value :: Expr -> Int
value e = eval e []


e = Add (Add (Val 2) (Val 3)) (Val 4)
