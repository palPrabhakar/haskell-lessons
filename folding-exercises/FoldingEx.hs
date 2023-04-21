module FoldingEx where


-- | list reverse
--
--  Examples:
--
-- >>> rev []
-- []
--
-- >>> rev "abcd"
-- "dcba"
--
-- >>> rev [1,2,3,4]
-- [4,3,2,1]
rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []


-- | list prefix
--
-- Examples:
--
-- >>> prefix []
-- []
--
-- >>> prefix [1]
-- [[1]]
--
-- >>> prefix [1,2,3]
-- [[1],[1,2],[1,2,3]]
prefix :: [a] -> [[a]]
prefix = foldr (\x acc -> [x] : map (x:) acc) []
