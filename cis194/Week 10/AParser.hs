{- CIS 194 HW 10 -}

module AParser where

import Control.Applicative
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1
first :: (a -> b) -> (a, c) -> (b, c)
first f p@(a,c) = (f a, c)

-- Essentially the fmap converts (String -> Maybe (a, String)) into (String -> Maybe (f a, String))
instance Functor Parser where
  fmap f (Parser g) = Parser (fmap convertParserOutput g)
    where
      convertParserOutput Nothing = Nothing
      convertParserOutput (Just t) = Just $ first f t
    
-- Exercise 2
-- The idea behind implementation is given in the hw
instance Applicative Parser where
  pure a = Parser output
    where
      output str = Just (a, str)
  (<*>) p1@(Parser f) p2@(Parser p) = Parser output
    where
      output str = 
        case (f str) of
          Nothing -> Nothing
          Just t1@(g, rem) ->
            case (p rem) of
              Nothing -> Nothing
              Just t2 -> Just $ first g t2

-- Exercise 3
abParser :: Parser (Char, Char)
abParser = (,) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (\c1-> (\c2 -> ())) <$> (char 'a') <*> (char 'b')

intPair :: Parser [Integer]
intPair = (\x1 -> (\x2 -> (\x3 -> [x1,x3]))) <$> posInt <*> char ' ' <*> posInt


-- Exercsie 4
-- class Applicative f => Alternative f where
--   empty :: f a
--   (<|>) :: f a -> f a -> f 

-- Empty is identity of Alternative, which generally represents failure
instance Alternative Parser where
  empty = Parser (\str -> Nothing)
  (<|>) p1@(Parser f) p2@(Parser g) = Parser output
    where
      output str =
        case out1 of
          Just _ -> out1
          Nothing ->
            case out2 of
              Nothing -> empty
              Just _ -> out2
            where 
              out2 = g str
        where 
          out1 = f str

-- Exercise 5
-- The follwing functions parses the datatype but doesn't return the parsed data, rather they return ()
parseInt :: Parser ()
parseInt = (\x -> ()) <$> posInt
parseUpper = (\x -> ()) <$> (satisfy isUpper)

-- Then we use the alternate operator to perform the logic
intOrUppercase :: Parser ()
intOrUppercase = parseInt <|> parseUpper
