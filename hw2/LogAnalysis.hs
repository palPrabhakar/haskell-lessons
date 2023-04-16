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
getErrorMsg (x:y:xs) = LogMessage (Error (read x :: Int)) (read y :: Int) (unwords xs)
getErrorMsg _ = Unknown "Error"

-- | create error info message
--
--  Examples:
--
-- >>> getInformationMsg ["42", "lol info"] == LogMessage Info 42 "lol info"
-- True
getInformationMsg :: [String] -> LogMessage
getInformationMsg (x:xs) = LogMessage Info (read x :: Int) (unwords xs)
getInformationMsg _ = Unknown "Info"

-- | create error warning message
--
--  Examples:
--
-- >>> getWarningMsg ["42", "lol warning"] == LogMessage Warning 42 "lol warning"
-- True
getWarningMsg :: [String] -> LogMessage
getWarningMsg (x:xs) = LogMessage Warning (read x :: Int) (unwords xs)
getWarningMsg _ = Unknown "Warning"

-- | parse log messages
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
      getMessage (x:xs) = case x of
        "I" -> getInformationMsg xs
        "W" -> getWarningMsg xs
        "E" -> getErrorMsg xs
        _ -> Unknown $ unwords (x:xs)



