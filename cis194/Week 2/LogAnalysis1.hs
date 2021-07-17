--{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

strsToStr :: [String] -> String
strsToStr (x:xs)
  | xs == []  = x
  | otherwise = x ++ " " ++ strsToStr(xs)

-- parseMessage :: String -> LogMessage
-- parseMessage msg
--   | mt == "I" = LogMessage Info ts rest
--   | mt == "W" = LogMessage Warning ts rest
--   | mt == "E" = LogMessage (Error forError) ts rest
--   | otherwise = Unknown msg
--   where strs = words msg
--         mt = strs !! 0
--         forError = 
--           case mt of
--                "E" -> (read (strs !! 2)) :: Int
--                _   -> 0
--         ts = 
--           case mt of 
--                "E" -> read (strs !! 3) :: Int
--                _   -> read (strs !! 2) :: Int
--         rest =
--           case mt of 
--                "E" -> unwords (drop 3 strs)
--                _   -> unwords (drop 2 strs)

getMessageType :: [String] -> MessageType
getMessageType strs 
  | mt == "I" = Info
  | mt == "W" = Warning
  | mt == "E" = Error (read (head (tail strs))) 
  | otherwise = Error (-1)
 where mt = head strs

parseMessage :: String -> LogMessage 
parseMessage msg 
  | mt /= (Error (-1)) = LogMessage mt ts rest
  | otherwise = Unknown msg
  where strs       = words msg
        mt         = getMessageType strs
        strsExclMt = case mt of 
                        (Error _) -> drop 2 strs
                        _         -> drop 1 strs
        ts         = read (head strsExclMt)
        rest       = unwords (drop 1 strsExclMt)
        
parse :: String -> [LogMessage]
parse str = parseLines (lines str)

parseLines :: [String] -> [LogMessage]
parseLines (l:ls) 
  | ls == []  = (parseMessage l) : []
  | otherwise = (parseMessage l) : parseLines (ls)

