{- Define the instance of Eq class for the following datatypes -}

data TisAnInteger =  TisAn Integer
instance Eq TisAnInteger where
   (==) (TisAn v) (TisAn v') = v == v'

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
   (==) (Two a b) (Two a' b') = 
        a == a' && b == b'

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
   (==) (TisAnInt a) (TisAnInt a') = a == a'
   (==) (TisAString s) (TisAString s') = s == s'
   (==) _ _ = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
   (==) (Pair a b) (Pair a' b') = a == b && b == a' && a' == b'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
   (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
   (==) (ThisOne v) (ThisOne v') = v == v'
   (==) (ThatOne v) (ThatOne v') = v == v'
   (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
   (==) (Hello v) (Hello v') = v == v'
   (==) (Goodbye v) (Goodbye v') = v == v'
   (==) _ _ = False

chk :: Eq b => (a -> b) -> a -> b -> Bool
