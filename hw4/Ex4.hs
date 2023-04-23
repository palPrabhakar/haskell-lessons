module Ex4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

-- | fun1'
--
--  Examples:
--
-- >>> fun1' [] == fun1 []
-- True
--
-- >>> fun1' [1, 2, 4] == fun1 [1, 2, 4]
-- True
--
-- >>> fun1' [1, 3, 5, 7, 9] ==  fun1 [1, 3, 5, 7, 9]
-- True
--
-- >>> fun1' [2, 4, 6, 8, 10, 12] ==  fun1 [2, 4, 6, 8, 10, 12]
-- True
fun1' :: [Integer] -> Integer
fun1' = foldr (\x acc -> if even x then (x - 2) * acc else acc) 1

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

testF n = takeWhile (> 1) (iterate (\x -> if even x then x `div` 2 else 3 * x + 1) n)

-- | fun2'
--
-- Examples:
--
-- >>> fun2' 1 == fun2 1
-- True
--
-- >>> fun2' 2 == fun2 2
-- True
--
-- >>> fun2' 3 == fun2 3
-- True
--
-- >>> fun2' 4 == fun2 4
-- True
fun2' :: Integer -> Integer
fun2' n = sum (filter even (takeWhile (> 1) (iterate (\x -> if even x then x `div` 2 else 3 * x + 1) n)))

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)


foldTree :: [a] -> Tree a
foldTree _ = Leaf


-- | Count the number of instance of element in a list
--
-- Examples:
--
-- >>> count 0 []
-- 0
--
-- >>> count 2 [1, 2, 3, 2, 4, 5, 2]
-- 3
--
-- >>> count True [True, False, False]
-- 1
count :: Eq a => a -> [a] -> Int
count elem = foldr (\x cnt -> if x == elem then cnt+1 else cnt) 0

-- | True if odd number of True in list
--
-- Examples:
--
-- >>> xor [False, True, False]
-- True
--
-- >>> xor [False, True, False, True]
-- False
--
-- >>> xor [True, True, True, False]
-- True
xor :: [Bool] -> Bool
xor list = odd (count True list)
