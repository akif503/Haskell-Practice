-- {-# OPTIONS_GHC -Wall #-}

-- Exercise 1: Wholemeal Programming
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x-2) * (fun1 xs)
    | otherwise = fun1 xs


fun1' :: [Integer] -> Integer
fun1' xs = foldr apply 1 xs 
    where apply x y = if even x then (x-2)*y else y

-- Adds all the even number the collatz series of n
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3*n+1)

fun2' :: Integer -> Integer
fun2' = sum . (filter even) . ((takeWhile (\x -> x /= 1)) . iterate apply)
    where apply x = if even x then (div x 2) else (3*x+1)

-- Exercise 2: Folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)
    
-- Function apply is a handler to call insert (with height)
-- height = floor(log_2(length xs))
-- enumerated = (0,a1), (1,a2), ... (i,ai), ...
foldTree :: [a] -> Tree a
foldTree xs = foldr apply Leaf enumerated
    where
        apply x tree = insert x (fromIntegral height :: Integer) tree
        height = (length . takeWhile (\x -> x /= 1) . iterate (\x -> div x 2) . length) xs
        enumerated = zip (take (length xs) $ iterate (\x -> x+1) 0) xs

-- Takes the one enumerated tuple (i, ai), height (n) & the tree
-- Fills in the Leaf first, if there is not Leaf then it propagates
-- the insertion to a sub-tree based on the tuple index (i), see the switch below
insert :: Integral a1 => (a1, a2) -> Integer -> Tree a2 -> Tree a2
insert tp h tree = case tree of
    Leaf -> Node h Leaf x Leaf
    Node n t1 nodeElem t2 -> case t1 of
        Leaf -> Node n (insert tp (n-1) t1) nodeElem t2
        _    -> case t2 of
            Leaf -> Node n t1 nodeElem (insert tp (n-1) t2)
            _    -> case switch of
                True -> Node n (insert tp (n-1) t1) nodeElem t2
                False -> Node n t1 nodeElem (insert tp (n-1) t2)
    where x = snd tp
          switch = even (fst tp) 

-- Exercise 3: More folds
xor :: [Bool] -> Bool
xor = foldr xor2 False
    where 
        xor2 False True = True
        xor2 True False = True
        xor2 _ _ = False
    
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

-- Optional: Implement foldl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr (flip f) z (reverse xs)


-- Exercise 4: Finding primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2] ++ (map (\x -> 2*x + 1) (filter (\x -> not $ elem x remList) [1..n]))
    where
        remList = [compose i j | i <- [1..n], j <- [1..i], compose i j <= n]
        compose i j = i + j + 2*i*j