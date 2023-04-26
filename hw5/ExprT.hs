module ExprT where

import Expr

data ExprT
  = Lit Integer
  | Add ExprT ExprT
  | Mul ExprT ExprT
  deriving (Show, Eq)

instance Expr ExprT where
  lit :: Integer -> ExprT
  lit = Lit

  add :: ExprT -> ExprT -> ExprT
  add e1 e2 = Add e1 e1

  mul :: ExprT -> ExprT -> ExprT
  mul e1 e2 = Mul e1 e1

reify :: ExprT -> ExprT
reify = id

