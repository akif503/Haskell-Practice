data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Semigroup (List a) where
    (<>) l Nil = l 
    (<>) Nil l = l
    (<>) (Cons a1 b1) l2 = Cons a1 (b1 <> l2)

instance Applicative List where
    pure a = Cons a Nil
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) a = (f <$> a) <> (fs <*> a)