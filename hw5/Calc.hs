module Calc where

import ExprT
import Parser

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


-- | evaluate strings
--
-- Examples:
--
-- >>> evalStr "1"
-- Just 1
--
-- >>> evalStr "(2+3)*4"
-- Just 20
--
-- >>> evalStr "2+3*4"
-- Just 14
--
-- >>> evalStr "2+3*"
-- Nothing
evalStr :: String -> Maybe Integer
evalStr strExp = parseExp Lit Add Mul strExp >>= \ex -> pure (eval ex)
