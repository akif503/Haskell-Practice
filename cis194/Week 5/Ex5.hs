{-# LANGUAGE TypeSynonymInstances #-}

import StackVM
import Parser
import qualified ExprT as T

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr 

testExp = parseExp lit add mul "(3*-4) + 5"