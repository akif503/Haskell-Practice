import Data.List (elemIndex)

-- 1.
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1..3] [4..6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1..3] [4..6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.
x1 :: Maybe Int
x1 = elemIndex 3 [1, 2, 3, 4, 5]

y1 :: Maybe Int
y1 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x1 <*> y1

-- 4. 
xs = [1, 2, 3]
ys = [4, 5, 6]

x2 :: Maybe Integer
x2 = lookup 3 $ zip xs ys

y2 :: Maybe Integer
y2 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x2 <*> y2

--
-- Identity
--
newtype Identity a = Identity a 
        deriving (Eq, Ord, Show)
    
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

--
-- Constant
--
newtype Constant a b = Constant { getConstant :: a }
        deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap f (Constant a) = Constant a

-- Not solved for pure
instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>) (Constant a) (Constant b) = Constant (a <> b)
