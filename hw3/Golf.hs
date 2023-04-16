module Golf where

import Data.List

-- | take every ith element from the list
--
--  Examples:
--
-- >>> sieve 1 "abcd"
-- "abcd"
--
-- >>> sieve 2 "abcdef"
-- "bdf"
--
-- >>> sieve 3 "abcdef"
-- "cf"
--
-- >>> sieve 0 "abcdef"
-- ""
--
-- >>> sieve 5 "abcd"
-- ""
sieve :: Int -> [a] -> [a]
sieve 0 _ = []
sieve n list = map fst (filter (\x -> snd x `rem` n == 0) (zip list [1..]))


-- | skip elements in a list
--
--  Examples:
--
-- >>> skips []
-- []
--
-- >>> skips "ABCD"
-- ["ABCD","BD","C","D"]
--
-- >>> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
--
-- >>> skips [1]
-- [[1]]
--
-- >>> skips [True, False]
-- [[True,False],[False]]
skips :: [a] -> [[a]]
skips [] = []
skips list = zipWith (curry (\x -> sieve (snd x) list)) list [1..]

