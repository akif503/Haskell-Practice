sumIt :: (Eq a, Num a) => a -> a
sumIt n
   | n == 0 || n == 1 = n
   | otherwise = (+) n $ sumIt (n-1)


-- Multiply using Addition
multWithSum :: (Eq a, Num a) => a -> a -> a
multWithSum a b = go a b 0 0
   where go a b acc count
          | count == b = acc
          | otherwise = go a b (acc+a) (count+1)


-- McCarthy 91 function
mc91 :: (Eq a, Ord a, Num a) => a -> a
mc91 n
   | n > 100 = n - 10
   | otherwise = mc91 $ mc91 (n + 11)

-- Handling zero division

data DividedResult = Result (Integer, Integer) | DividedByZero deriving Show

-- Returns quotient and remainder
db :: Integer -> Integer -> DividedResult
db _ 0 = DividedByZero
db num denom = go num denom 0
  where go n d count
         | n < d = Result (count, n)
         | otherwise = go (n-d) d (count + 1)

-- GCD
mgcd :: Integral a => a -> a -> a
mgcd n r
  | r == 0 = n
  | otherwise = mgcd r $ mod n r

