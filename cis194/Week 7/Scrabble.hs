module Scrabble where

newtype Score = Score Int
  deriving (Eq, Ord, Show)

instance Semigroup Score where
    (<>) (Score a) (Score b) = Score $ a + b

instance Monoid Score where
    mempty = Score 0
    mappend = (<>)

getScore :: Score -> Int
getScore (Score n) = n

score :: Char -> Score 
score letter
    | letter == 'A' || letter == 'a' = Score 1
    | letter == 'B' || letter == 'b' = Score 3
    | letter == 'C' || letter == 'c' = Score 3
    | letter == 'D' || letter == 'd' = Score 2
    | letter == 'E' || letter == 'e' = Score 1
    | letter == 'F' || letter == 'f' = Score 4
    | letter == 'G' || letter == 'g' = Score 2
    | letter == 'H' || letter == 'h' = Score 4
    | letter == 'I' || letter == 'i' = Score 1
    | letter == 'J' || letter == 'j' = Score 8
    | letter == 'K' || letter == 'k' = Score 5
    | letter == 'L' || letter == 'l' = Score 1
    | letter == 'M' || letter == 'm' = Score 3
    | letter == 'N' || letter == 'n' = Score 1
    | letter == 'O' || letter == 'o' = Score 1
    | letter == 'P' || letter == 'p' = Score 3
    | letter == 'Q' || letter == 'q' = Score 10
    | letter == 'R' || letter == 'r' = Score 1
    | letter == 'S' || letter == 's' = Score 1
    | letter == 'T' || letter == 't' = Score 1
    | letter == 'U' || letter == 'u' = Score 1
    | letter == 'V' || letter == 'v' = Score 4
    | letter == 'W' || letter == 'w' = Score 4
    | letter == 'X' || letter == 'x' = Score 8
    | letter == 'Y' || letter == 'y' = Score 4
    | letter == 'Z' || letter == 'z' = Score 10
    | otherwise = Score 0

scoreString :: String -> Score
scoreString [] = Score 0
scoreString (x:xs) = (score x) <> (scoreString xs)

