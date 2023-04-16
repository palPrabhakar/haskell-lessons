{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- | create error log message
--
--  Examples:
--
-- >>> getErrorMsg ["2", "42", "lol error"] == LogMessage (Error 2) 42 "lol error"
-- True
getErrorMsg :: [String] -> LogMessage
getErrorMsg (x : y : xs) = LogMessage (Error (read x :: Int)) (read y :: Int) (unwords xs)
getErrorMsg _ = Unknown "Error"

-- | create error info message
--
--  Examples:
--
-- >>> getInformationMsg ["42", "lol info"] == LogMessage Info 42 "lol info"
-- True
getInformationMsg :: [String] -> LogMessage
getInformationMsg (x : xs) = LogMessage Info (read x :: Int) (unwords xs)
getInformationMsg _ = Unknown "Info"

-- | create error warning message
--
--  Examples:
--
-- >>> getWarningMsg ["42", "lol warning"] == LogMessage Warning 42 "lol warning"
-- True
getWarningMsg :: [String] -> LogMessage
getWarningMsg (x : xs) = LogMessage Warning (read x :: Int) (unwords xs)
getWarningMsg _ = Unknown "Warning"

-- | create parse log messages
--
--  Examples:
--
-- >>> parseMessage "W 42 lol warning" == LogMessage Warning 42 "lol warning"
-- True
--
-- >>> parseMessage "I 42 lol info" == LogMessage Info 42 "lol info"
-- True
--
-- >>> parseMessage "E 2 42 lol error" == LogMessage (Error 2) 42 "lol error"
-- True
--
-- >>> parseMessage "X lol unknown error" == Unknown "X lol unknown error"
-- True
--
-- >>> parseMessage "lol unknown error" == Unknown "lol unknown error"
-- True
--
-- >>> parseMessage "" == Unknown "log message"
-- True
parseMessage :: String -> LogMessage
parseMessage msg = getMessage $ words msg
  where
    getMessage :: [String] -> LogMessage
    getMessage [] = Unknown "log message"
    getMessage (x : xs) = case x of
      "I" -> getInformationMsg xs
      "W" -> getWarningMsg xs
      "E" -> getErrorMsg xs
      _ -> Unknown $ unwords (x : xs)

parse :: String -> [LogMessage]
parse logs = map parseMessage (lines logs)

-- | insert log messages into message tree
--
--  Examples:
--
-- >>> insert (parseMessage "X 42 lol warning") Leaf == Leaf
-- True
--
-- >>> insert (parseMessage "W 42 lol warning") Leaf == Node Leaf (parseMessage "W 42 lol warning") Leaf
-- True
--
-- >>> insert (parseMessage "I 20 lol info") (Node Leaf (parseMessage "W 42 lol warning") Leaf) == Node (Node Leaf (parseMessage "I 20 lol info") Leaf) (parseMessage "W 42 lol warning") Leaf
-- True
--
-- >>> insert (parseMessage "E 2 55 lol error") (Node (Node Leaf (parseMessage "I 20 lol info") Leaf) (parseMessage "W 42 lol warning") Leaf) == (Node (Node Leaf (parseMessage "I 20 lol info") Leaf) (parseMessage "W 42 lol warning") (Node Leaf (parseMessage "E 2 55 lol error") Leaf))
-- True
insert :: LogMessage -> MessageTree -> MessageTree
insert (LogMessage lmty lmtt lmm) Leaf = Node Leaf (LogMessage lmty lmtt lmm) Leaf
insert (LogMessage lmty lmtt lmm) (Node lt (LogMessage rmty rmtt rmm) rt)
  | lmtt < rmtt = Node (insert (LogMessage lmty lmtt lmm) lt) (LogMessage rmty rmtt rmm) rt
  | otherwise = Node lt (LogMessage rmty rmtt rmm) (insert (LogMessage lmty lmtt lmm) rt)
insert (Unknown _) mt = mt
insert _ _ = Leaf

-- | build message tree
--
--  Examples:
--
-- >>> build [parseMessage "W 42 lol warning"] == Node Leaf (parseMessage "W 42 lol warning") Leaf
-- True
--
-- >>> build [parseMessage "W 42 lol warning", parseMessage "I 20 lol info"]  == Node (Node Leaf (parseMessage "I 20 lol info") Leaf) (parseMessage "W 42 lol warning") Leaf
-- True
--
-- >>> build [parseMessage "W 42 lol warning", parseMessage "I 20 lol info", parseMessage "E 2 55 lol error"] == Node (Node Leaf (parseMessage "I 20 lol info") Leaf) (parseMessage "W 42 lol warning") (Node Leaf (parseMessage "E 2 55 lol error") Leaf)
-- True
build :: [LogMessage] -> MessageTree
build [] = Leaf
build msg = buildTree msg Leaf
  where
    buildTree :: [LogMessage] -> MessageTree -> MessageTree
    buildTree [] mt = mt
    buildTree (x : xs) mt = buildTree xs $ insert x mt
