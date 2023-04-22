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
