{-# LANGUAGE TypeSynonymInstances #-}
module Program where

import StackVM
import Parser
import Expr

instance Expr Program where
  lit :: Integer -> Program
  lit n = [PushI n]

  add :: Program -> Program -> Program
  add e1 e2 = e2 ++ e1 ++ [Add]

  mul :: Program -> Program -> Program
  mul e1 e2 = e2 ++ e1 ++ [Mul]


compile :: String -> Maybe Program
compile = parseExp lit add mul

-- | run programs
--
-- Examples:
--
-- >>> run "42"
-- Just (Right (IVal 42))
--
-- >>> run "2+4"
-- Just (Right (IVal 6))
--
-- >>> run "2+4*3"
-- Just (Right (IVal 14))
--
-- >>> run "(2*4)+(4*3)"
-- Just (Right (IVal 20))
run :: String -> Maybe (Either String StackVal)
run str = compile str >>= \ex -> pure (stackVM ex)


-- testExp :: Expr a => Maybe a
-- testExp = parseExp lit add mul "(3 * -4) + 5"
-- testExp = parseExp lit add mul "(3 * -4) + (2 * 5)"

-- testProgram = testExp :: Maybe Program

