-- wiki: https://wiki.haskell.org/99_questions/1_to_10
module ProblemSet1 where

-- Problem 1
myLast :: (Eq a) => [a] -> a
-- Don't sent an empty list
myLast (x:xs)
    | xs == [] = x
    | otherwise = myLast xs


-- Problem 2
-- Find the second last element
myButLast :: Eq a => [a] -> a
-- Give a list with at least 2 elememts
myButLast (x1:x2:xs)
    | xs == [] = x1
    | otherwise = myButLast (x2:xs)


-- Problem 3
-- Find the k-th element of a list. Indexing with 1
elementAt :: [a] -> Integer -> a
elementAt list n = go list n 1
    where go (x:xs) n count
           | count == n = x
           | otherwise = go xs n (count + 1)

-- Problem 4
-- Find the number of element in a list
myLength :: (Eq a) => [a] -> Integer
myLength [] = 0
myLength list = go list 1
   where go (x:xs) count
          | xs == [] = count
          | otherwise = go xs (count+1)

-- Problem 5
-- Reverse a list
myReverse :: (Eq a) => [a] -> [a]
myReverse [] = []
myReverse list = go list []
   where go (x:xs) out
          | xs == [] = x:out
          | otherwise = go xs (x:out)

-- Problem 6
-- Is Palindrome?
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome list = go list (reverse list)
   where go (x:xs) (y:ys)
          | xs == [] = True
          | x /= y = False
          | otherwise = go xs ys

-- *** Problem 7
-- Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a] deriving (Show, Eq)
flatten :: (Eq a) => NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ (flatten (List xs))


-- Problem 8
-- Eliminate consecurive duplicates
compress :: Eq a => [a] -> [a]
compress [] = []
compress list = go (tail list) [head list] (head list)
   where go (x:xs) out prev
          | xs == [] = if prev /= x then (out ++ [x]) else out
          | prev /= x = go xs (out ++ [x]) x
          | otherwise = go xs out x

        
-- Problem 9
-- Pack consecutive duplicates of a list elements into a sublist

-- Makes the first consecutive elements and returns the rest
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

-- Problem 10
encode :: (Eq a) => [a] -> [(Integer, a)]
encode xs = go (pack xs) []
    where go (x:xs) out
           | xs == [] = out ++ [(myLength x, head x)]
           | otherwise = go xs (out ++ [(myLength x, head x)])
