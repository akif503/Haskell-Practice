-- Pg 1004
-- 1.
a = fmap (+1) $ read "[1]" :: [Int]

-- 2.
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
c = (*2) . (\x -> x - 2) 

-- 4.
d = ((return '1' ++) . show) . (\x -> [x, (x+1)..(x+5)])

-- 5. GREAT EXAMPLE of why functors matter
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read $ fmap ("123" ++) $ fmap show ioi
    in fmap (*3) changed


-- Exercise: Possibly
data Possibly a =
      LolNope
    | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)


-- Short Exercise : Pg - 1023
data Sum a b =
      First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

