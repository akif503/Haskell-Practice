{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- Exercise 1
fib :: Integer -> Integer 
fib n
    | n == 0 || n == 1 = n
    | otherwise = (fib (n-2)) + (fib (n-1))

fibs1 = map fib [0..]

-- Exercise 2
-- An efficient implementation of an infinite fibonacci list
fibs2 :: [Integer]
fibs2 = [0,1] ++ go (0,1)
    where go p2 = [res] ++ (go (snd p2, res))
            where res = (fst p2) + (snd p2)

-- Exercise 3
data Stream a = Cons a (Stream a)

instance (Show a) => Show (Stream a) where 
    show x = show (take 20 (streamToList x))

streamToList :: Stream a -> [a]
streamToList (Cons a st) = a : (streamToList st)

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a xs) = Cons (f a) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5
nats :: Stream Integer 
nats = streamFromSeed (\x -> x + 1) 0

ruler :: Stream Integer
ruler = go 1 
    where go n = (Cons (maxPowOf2 n) (go (n+1)))
rulerB = go 1 
    where go n = (Cons (maxPowOf2B n) (go (n+1)))

maxPowOf2 :: Integer -> Integer
maxPowOf2 n = go n p
    where 
        p = floor (logBase 2.0 (fromInteger n))
        go n p
            | p == 0 = 0
            | otherwise = case n `mod` (2^p) == 0 of
                True -> p
                False -> (go n (p - 1))

-- Without using any divisibility rule, although log uses division ...
-- This uses the fact that the right most 1 (lowest 2^k) in the
-- binary representation of n is the 2^k in its prime factorization.
maxPowOf2B n = go n 
    where 
        go n
          | rem == 0 = p
          | otherwise = go rem
            where
                p = floor (logBase 2.0 (fromInteger n))
                rem = n - (2 ^ p)


-- Exercise 6
x :: Stream Integer
x = (Cons 0 (Cons 1 (streamRepeat 0)))

instance Num (Stream Integer) where
    fromInteger n = (Cons n (streamRepeat 0))
    negate (Cons a xs) = (Cons (-a) (negate xs))
    (+) (Cons a1 str1) (Cons a2 str2) = (Cons (a1+a2) (str1 + str2))
    (*) (Cons a0 ap) (Cons b0 bp) = 
                (Cons (a0*b0) 
                ((streamMap (*a0) bp) + (ap*(Cons b0 bp))))

instance Fractional (Stream Integer) where
    (/) (Cons a0 ap) (Cons b0 bp) = q
            where
                q = (Cons (div a0 b0)
                    (streamMap (\x -> div x b0) (ap - (q*bp))))

-- CRAZY!!!
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7
-- Fib with matrix exponentiation
data Matrix a = Matrix (a,a) (a,a)
    deriving (Show, Eq)

instance (Integral a) => Num (Matrix a) where
    (*) (Matrix m1r1 m1r2) (Matrix m2r1 m2r2) =
            Matrix  (multPairs m1r1 m2c1, multPairs m1r1 m2c2)
                    (multPairs m1r2 m2c1, multPairs m1r2 m2c2)
        where 
            m1c1 = getCol 1 (m1r1, m1r2)
            m1c2 = getCol 2 (m1r1, m1r2)
            m2c1 = getCol 1 (m2r1, m2r2)
            m2c2 = getCol 2 (m2r1, m2r2)
            getCol n (row1, row2) = 
                case n of
                    1 -> (fst row1, fst row2)
                    2 -> (snd row1, snd row2)
            multPairs (a1,b1) (a2,b2) = (a1*a2 + b1*b2)

-- Computes the nth fib in O(logn) time
fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = res
    where fn@(Matrix _ (res, _)) = (Matrix (1,1) (1,0))^n