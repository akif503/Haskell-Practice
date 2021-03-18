-- Helper functions
packFirst :: (Eq a) => [a] -> ([a], [a])
packFirst [] = ([], [])
packFirst xs = go (tail xs) [head xs] (head xs)
   where go (x:xs) out prev
          | xs == [] = (x:out, [])
          | x /= prev = (out, x:xs)
          | otherwise = go xs (x:out) x

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack list = join list []
    where join list packed
           | list == [] = packed
           | otherwise = join rest (packed ++ [elem])
           where
            t1 = packFirst list
            elem = fst t1
            rest = snd t1

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = go (pack xs) []
    where go (x:xs) out
           | xs == [] = out ++ [(length x, head x)]
           | otherwise = go xs (out ++ [(length x, head x)])


-- Problem 11
data Encoding11 a = Multiple Int a | Single a deriving (Show, Eq)
encodeModified :: Eq a => [a] -> [Encoding11 a]
encodeModified xs = go (encode xs) []
    where go (x:xs) out
            | xs == [] = out ++ [(pickOut x)]
            | otherwise = go xs (out ++ [(pickOut x)])

pickOut :: Eq a => (Int, a) -> Encoding11 a
pickOut (n,a)
   | n == 1 = Single a
   | otherwise = Multiple n a

-- Problem 12 (Decode ^)

decodeModified :: Eq a => [Encoding11 a] -> [a]
decodeModified [] = []
decodeModified (x:xs)
   | xs == [] = go x
   | otherwise = (go x) ++ (decodeModified xs)
   where
       go (Single a) = [a]
       go (Multiple n a) = take n $ repeat a

-- Problem 13
-- encode direct problem 11 but don't use the helper functions
-- You can use pickOut tho
encodeDirect :: Eq a => [a] -> [Encoding11 a]
encodeDirect [] = []
encodeDirect (x:xs)
    | xs == [] = [pickOut (1,x)]
    | otherwise = [pickOut bundle] ++ (encodeDirect next)
    where
       t1 = go xs 1 x
       next = fst t1
       bundle = snd t1
       go (x:xs) count prev
          | x /= prev || xs == [] = (x:xs, (count, prev))
          | otherwise = go xs (count + 1) x

-- Problem 14 
-- Duplicate
dupli :: Eq a => [a] -> [a]
dupli (x:xs)
    | xs == [] = x:x:[]
    | otherwise = x:x:(dupli xs)
              
-- Problem 15
repli (x:xs) n
   | xs == [] = (take n $ repeat x)
   | otherwise =  (take n $ repeat x) ++ repli xs n

-- Problem 16
-- Drop every nth element
dropEvery xs n = reverse $ go xs 1 []
   where
      go (x:xs) count out
         | xs == [] = if count `mod` n == 0 then out else (x:out)
         | otherwise = if count `mod` n == 0 
                       then go xs (count + 1) out
                       else go xs (count + 1) (x:out)

-- Problem 19
-- Rotate a list by n place to the left
rotate xs n = 
   case n > 0 of
      True -> go [] xs n
      False -> go [] xs (n `mod` len)
   where
      len = length xs
      go hold (x:xs) count
         | xs == [] = go [] ((x:xs) ++ hold) count
         | count == 0 = (x:xs) ++ hold
         | otherwise =
            go (hold ++ [x]) xs (count - 1)