module Calc where

import ExprT

-- | evaluate expression
--
--  Examples:
--
-- >>> eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
-- 20
eval :: ExprT -> Integer
eval (Lit num) = num
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
