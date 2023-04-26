module Expr where

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr Integer where
  lit :: Integer -> Integer
  lit n = n

  add :: Integer -> Integer -> Integer
  add n1 n2 = n1 + n2

  mul :: Integer -> Integer -> Integer
  mul n1 n2 = n1 * n2

instance Expr Bool where
  lit :: Integer -> Bool
  lit n = n > 0

  add :: Bool -> Bool -> Bool
  add b1 b2 = b1 || b2

  mul :: Bool -> Bool -> Bool
  mul b1 b2 = b1 && b2

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit :: Integer -> MinMax
  lit = MinMax

  add :: MinMax -> MinMax -> MinMax
  add (MinMax m1) (MinMax m2) = MinMax (max m1 m2)

  mul :: MinMax -> MinMax -> MinMax
  mul (MinMax m1) (MinMax m2) = MinMax (min m2 m2)

instance Expr Mod7 where
  lit :: Integer -> Mod7
  lit n = Mod7 (n `mod` 7)

  add :: Mod7 -> Mod7 -> Mod7
  add (Mod7 m1) (Mod7 m2) = Mod7 ((m1 + m2) `mod` 7)

  mul :: Mod7 -> Mod7 -> Mod7
  mul (Mod7 m1) (Mod7 m2) = Mod7 ((m2 * m2) `mod` 7)
