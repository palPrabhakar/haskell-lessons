{-# LANGUAGE TypeSynonymInstances #-}
module Program where

import StackVM
import Expr

instance Expr Program where
  lit :: Integer -> Program
  lit n = [PushI n]

  add :: Program -> Program -> Program
  add e1 e2 = [Add] ++ e1 ++ e2

  mul :: Program -> Program -> Program
  mul e1 e2 = [Mul] ++ e1 ++ e2



