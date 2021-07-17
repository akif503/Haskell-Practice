{-# LANGUAGE FlexibleInstances #-}

-- Rearranging the arguments to the type constructor 
-- of the datatype so the Functor instances work

-- 1.
data Sum a b = First a | Second b

instance Functor (Sum e) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)

-- 2.
data Company a b c = DeepBlue a c | Something b

instance Functor (Company e e') where
    fmap f (Something b) = Something b
    fmap f (DeepBlue a c) = DeepBlue a (f c)

-- 3. 
data More a b =
      L a b a
    | R b a b
    deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L a (f b) a'
    fmap f (R b a b') = R (f b) a (f b')

-- Write Functor instances for the following datatypes
-- 1.
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

-- 2. 
{-
data K a b = K a
-}


instance Functor (K a) where
    fmap f (K a) = K a

-- 3. Don't know how to do it, gotta review
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype K a b = K a

--instance Functor (Flip K a) where
--    fmap f (Flip fba) = Flip (fmap f fba)

-- 4. 
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

-- 5.
data LiftItOut f a = LiftItOut (f a)

instance (Functor f) => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6.
data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 9.
data List a = Nil | Cons a (List a) deriving (Show)

instance Functor (List) where
    fmap _ Nil = Nil
    fmap f (Cons a l) = Cons (f a) (fmap f l)

-- 10.
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Show)

instance Functor GoatLord where 
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1)
                                          (fmap f g2)
                                          (fmap f g3)
                                    
-- 11. HARD
data TalkToMe a = 
    Halt
  | Print String a
  | Read (String -> a)

