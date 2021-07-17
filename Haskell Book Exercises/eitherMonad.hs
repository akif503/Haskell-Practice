import Control.Monad (join)

data Sum a b = First a | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    (<*>) (First f) _ = First f
    (<*>) (Second f) r = fmap f r

instance Monad (Sum a) where
    -- (>>=) r f = join $ fmap f r
    First a >>= _ = First a
    Second b >>= f = (f b)