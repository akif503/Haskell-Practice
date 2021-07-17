{-# LANGUAGE FlexibleInstances #-}


import qualified Data.Map as M
-- Exercise 6

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String 
  deriving (Show, Eq)

instance HasVars VarExprT where
    var st = Var st

instance Expr VarExprT where 
    lit n = Lit n
    add ex1 ex2 = Add ex1 ex2
    mul ex1 ex2 = Mul ex1 ex2

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var st = M.lookup st

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n = f 
        where f m = Just n
    add ex1 ex2 = f
        where 
            f m = 
                case (ex1 m) of
                    Just n1 -> case (ex2 m) of 
                        Nothing -> Just n1
                        Just n2 -> Just (n1 + n2)
                    Nothing -> ex2 m
    mul ex1 ex2 = f
        where 
            f m = 
                case (ex1 m) of
                    Just n1 -> case (ex2 m) of 
                        Nothing -> Just n1
                        Just n2 -> Just (n1 * n2)
                    Nothing -> ex2 m

withVars :: [(String, Integer)] 
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs