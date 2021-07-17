-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Calc where 


import qualified Data.Map as M
import ExprT
import Parser
-- import qualified StackVM as S


-- Exercise 1
{-
eval :: T.ExprT -> Integer 
eval (T.Lit n) = n
eval (T.Add expr1 expr2) = (eval expr1) + (eval expr2)
eval (T.Mul expr1 expr2) = (eval expr1) * (eval expr2)

-- Test
test1 = (eval (T.Mul (T.Add (T.Lit 2) (T.Lit 3)) (T.Lit 4)) == 20)
-}

eval :: ExprT -> Integer 
eval (Lit n) = n
eval (Add expr1 expr2) = (eval expr1) + (eval expr2)
eval (Mul expr1 expr2) = (eval expr1) * (eval expr2)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr xs = case parseExp Lit Add Mul xs of 
    Just expr -> Just (eval expr)
    Nothing -> Nothing

-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where 
    lit n = Lit n
    add ex1 ex2 = Add ex1 ex2
    mul ex1 ex2 = Mul ex1 ex2

reify :: ExprT -> ExprT
reify = id

-- test2 = reify $ mul (add (lit 2) (lit 3)) (lit 4)

-- Exercise 4
newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit n = n
    add n1 n2 = n1 + n2
    mul n1 n2 = n1*n2

instance Expr Bool where
    lit b = b > 0
    add b1 b2 = case b1 of
        True -> True
        False -> case b2 of
            True -> True
            False -> False
    mul b1 b2 = case b1 of
        False -> False
        True -> case b2 of
            False -> False
            True -> True

instance Expr MinMax where
    lit a = MinMax a
    add (MinMax a1) (MinMax a2) = MinMax (max a1 a2)
    mul (MinMax a1) (MinMax a2) = MinMax (min a1 a2)

instance Expr Mod7 where
    lit n = Mod7 (mod n 7)
    add (Mod7 n1) (Mod7 n2) = Mod7 (mod (n1 + n2) 7)
    mul (Mod7 n1) (Mod7 n2) = Mod7 (mod (n1 * n2) 7)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"
testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

-- Exercise 5
