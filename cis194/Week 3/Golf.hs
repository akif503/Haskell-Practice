module Golf where

-- Exercise 1: Hopscotch
skips :: Eq a => [a] -> [[a]]
skips xs = map (skipN xs 0) [1..(length xs)]

skipN (x:xs) count n
    | xs == [] = pick
    | otherwise = pick ++ (skipN xs (count + 1) n)
    where
        pick = if ((count+1) `mod` n == 0) then [x] else []

-- Exercise 2: Local Maxima
localMaxima :: [Integer] -> [Integer]
localMaxima xs = if (length xs) > 2 then go xs else []
    where go (x1:x2:x3:xs) = pick ++ (localMaxima (x2:x3:xs))
            where pick = if ((x1 < x2) && (x2 > x3))
                         then [x2] else []

-- Exercise 3: Histogram
histogram :: [Integer] -> String
histogram xs = go xs height
    where 
        height = maximum $ map (countN xs) xs
        go xs h
            | h == 1 = ret ++ bp
            | otherwise = ret ++ (go xs (h - 1))
            where bp = (take 10 $ repeat '=') ++ "\n"
                        ++ (concat $ map show [0..9]) ++ "\n"
                  ret = (map (putMark xs h) [0..9]) ++ "\n"

putMark :: [Integer] -> Integer -> Integer -> Char
putMark xs h n = if h <= (countN xs n) then '*' else ' '

countN :: [Integer] -> Integer -> Integer
countN xs n = (fromIntegral $ length $ filter (\x -> x == n) xs)
