import Data.Time

data DatabaseItem = 
    DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [
    DbDate (UTCTime 
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123)),
           DbNumber 9001,
           DbString "Hello, World!",
           DbDate (UTCTime
                  (fromGregorian 1921 5 1)
                  (secondsToDiffTime 34123))
    ]

-- Extracts all UTC value
filterDbData :: [DatabaseItem] -> [UTCTime]
filterDbDataOnce a b =
    case a of
        (DbDate t) -> t:b
        _          -> b
filterDbData xs = foldr filterDbDataOnce [] xs

-- Extractsall the Numbers
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumberOnce a b =
    case a of 
        (DbNumber x) -> x:b
        _            -> b
filterDbNumber xs = foldr filterDbNumberOnce [] xs

-- Extract the most recent date
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr go (head y) y
    where
        y = filterDbData xs
        go a b = if a > b then a else b

-- Sum all the DbNumber values
sumDb :: [DatabaseItem] -> Integer
sumDb xs = sum $ filterDbNumber xs

-- Average
avgDb :: [DatabaseItem] -> Double
avgDb xs = ss / size
    where
        nums = filterDbNumber xs
        ss = (fromIntegral $ sumDb xs) :: Double
        size = (fromIntegral (foldr (\a b -> b + 1) 0 nums)) :: Double