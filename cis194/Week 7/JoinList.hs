-- TODO: create a method for inserting 
-- into a balanced binary tree
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Main where

import Data.Monoid 
import Sized
import Scrabble
import StringBuffer
import Editor
import Buffer


data JoinList m a = 
      Empty
    | Single m a 
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)
  
-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) k Empty = k
(+++) Empty k = k
(+++) j1@(Single m1 _) j2@(Single m2 _) = Append (m1 <> m2) j1 j2
(+++) j1@(Append m1 _ _) j2@(Single m2 _) = Append (m1 <> m2) j1 j2
(+++) j1@(Single m1 _) j2@(Append m2 _ _) = Append (m1 <> m2) j1 j2
(+++) j1@(Append m1 _ _) j2@(Append m2 _ _) = Append (m1 <> m2) j1 j2

-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ idx _ | idx < 0 = Nothing
indexJ idx (Single _ a) 
  | idx == 0 = Just a
  | otherwise = Nothing
indexJ idx (Append m l1 l2)
  | n1 == 0 = indexJ idx l2
  | n2 == 0 = indexJ idx l1
  | idx < n1 = indexJ idx l1
  | otherwise = indexJ (idx - n1) l2
  where 
    n1 = getSize $ size (tag l1)
    n2 = getSize $ size (tag l2)

-- For testing
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Exercise 3
scoreLine :: String -> JoinList Score  String
scoreLine xs = Single (scoreString xs) xs

-- Exercise 4
strToJl :: String -> JoinList (Score, Size) String
strToJl xs = Single (scoreString xs, Size 1) xs

-- Very inefficient
-- You gotta implement an insertion method into a balanced tree
-- such that the order of append is maintained
compose :: [String] -> JoinList (Score, Size) String
compose [] = Empty
compose xs
  | n == 1 = Single (scoreString x, Size 1) x
  | otherwise = (compose $ fst bifurcate) +++ (compose $ snd bifurcate)
  where
    x = head xs
    n = length xs
    bifurcate = go 0 (div (length xs) 2) [] xs
    go count upto sofar (x:xs)
      | count == upto = (sofar, x:xs)
      | otherwise = go (count + 1) upto (sofar ++ [x]) xs

replaceLine' :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
replaceLine' _ _ Empty = Empty
replaceLine' n l (Single m x)
  | n == 0 = strToJl l
  | otherwise = Empty
replaceLine' n l jl@(Append m l1 l2)
  | n1 == 0 = replaceLine' n l l2
  | n2 == 0 = replaceLine' n l l1
  | n < n1 = (replaceLine' n l l1) +++ l2
  | otherwise = l1 +++ (replaceLine' (n - n1) l l2)
  where 
    n1 = getSize $ size (tag l1)
    n2 = getSize $ size (tag l2)


-- Here argument b is a buffer, so an instance of join-list.
-- Also, b would be of the form dictated by fromString function.
-- For example, if you keep lines at each of the leaf, or words at each of the leaf
-- so, the buffer would be different based on the definition of fromString

instance Buffer (JoinList (Score, Size) (String)) where
  toString = concat . jlToList 
  fromString = compose . lines
  line = indexJ
  replaceLine = replaceLine'
  numLines = getSize . snd . tag
  value = getScore . fst . tag

buf = [ "This buffer is for notes you don't want to save, and for"
        , "evaluation of steam valve coefficients."
        , "To load a different file, type the character L followed"
        , "by the name of the file."
      ]

main = runEditor editor $ compose buf
