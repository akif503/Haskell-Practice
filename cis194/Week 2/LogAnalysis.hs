--{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage "" = Unknown ""
parseMessage xs = parseMessage' $ words xs

parseMessage' :: [String] -> LogMessage
parseMessage' (x:xs)
    | x == "I" = LogMessage Info (getTimeStamp xs) (concatStrs $ tail xs)
    | x == "W" = LogMessage Warning (getTimeStamp xs) (concatStrs $ tail xs)
    | x == "E" = LogMessage (Error (read $ head xs)) (getTimeStamp xs) (concatStrs $ tail $ tail xs)
    | otherwise = Unknown $ concatStrs (x:xs)
    where
        getTimeStamp xs = read $ head xs
        concatStrs str = case str of
            [] -> []
            _  -> ret str
            where 
                ret (x:xs)
                   | xs == [] = x
                   | otherwise = x ++ " " ++ (concatStrs xs)
        
parse :: String -> [LogMessage]
parse xs = parse' (lines xs)
    where 
        parse' (x:xs)
            | xs == [] = [parseMessage x]
            | otherwise = [parseMessage x] ++ (parse' xs)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert (LogMessage mt ts st) tree = case tree of
    Leaf -> Node Leaf msg Leaf
    (Node left (LogMessage mt2 ts2 st2) right) ->
        case (ts < ts2) of
            True -> (Node (insert msg left) msg2 right)
            False -> (Node left (LogMessage mt2 ts2 st2) (insert msg right))
        where
            msg2 = (LogMessage mt2 ts2 st2)
    where 
        msg = (LogMessage mt ts st)


build :: [LogMessage] -> MessageTree
build (x:xs)
    | xs == [] = insert x Leaf
    | otherwise = insert x (build xs)


inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
    Leaf -> []
    (Node left msg right) -> (inOrder left) ++ [msg] ++ (inOrder right)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = reverse $ go (inOrder $ build xs)
  where
    go (x:xs)
        | xs == [] = ret
        | otherwise = ret ++ (go xs)
        where 
            ret = case x of
                LogMessage (Error n) _ msg -> if n > 50 
                                              then [msg] else []
                _ -> []

-- Optional: Story
getInfoMsgs :: [LogMessage] -> [String]
getInfoMsgs xs = go (inOrder $ build xs)
  where
    go (x:xs)
        | xs == [] = ret
        | otherwise = ret ++ (go xs)
        where 
            ret = case x of
                LogMessage Info _ msg -> subret msg
                LogMessage Warning _ msg -> subret msg
                LogMessage (Error _) _  msg -> subret msg
                _ -> []
            subret msg = if not (numericString msg)
                     then [msg] else []

numericString :: String -> Bool
numericString str = go [0..9]
    where go (x:xs)
            | xs == [] = elem (show x !! 0) str
            | otherwise = if not exists
                          then (go xs) else exists
            where
                exists = (elem (show x !! 0) str)
