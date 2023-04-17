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

-- | find local maxima  in a 3 elem list
--
-- Examples:
--
-- >>> isLocalMaxima [1,2,3]
-- []
--
-- >>> isLocalMaxima [3,4,2]
-- [4]
isLocalMaxima :: [Integer] -> [Integer]
isLocalMaxima [x,y,z] =  [y | x < y && y > z]
isLocalMaxima _ = []

-- | skip elements in a list
--
--  Examples:
--
-- >>> localMaxima [2,9,5,6,1]
-- [9,6]
--
-- >>> localMaxima [2,3,4,1,5]
-- [4]
--
-- >> localMaxima [1,2,3,4,5]
-- []
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima list = isLocalMaxima (take 3 list) ++ localMaxima (drop 1 list)

