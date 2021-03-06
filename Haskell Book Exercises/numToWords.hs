-- Exercise of page 453 (501)
module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"


digits :: Int -> [Int]
digits n
  | div n 10 == 0 = [n]
  | otherwise = (++) (digits (div n 10)) [mod n 10] 

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
{-
 - The intersperse method inserts element a between a list of [a], and returns the resulting list.
 - After that concat joins all the inner list into a single list (which is in our case a string. 
-}
 
