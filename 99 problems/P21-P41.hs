-- Generate combinations of K distinct 
-- objects choosen from N elements.
combinations (x:xs) r = 
    case (r-1) <= (length xs) of
        True -> if r >= 0 
            then if r > 0 
                then (go [x] xs 1) ++ (go [] xs 0)
                else []
            else error "r has to positive or 0"
        False -> error "r must be less than or equal to n."
    where
        go taken left count
           | left == [] = 
               case count == r of
                  True -> [taken]
                  False -> []
           | count == r = [taken]
           | otherwise = (++) b1 b2
           where
               b1 = (go ((head left):taken) (tail left) (count+1))
               b2 = (go taken (tail left) count)

-- All combinations
allComb (x:xs) = (go [x] xs 1) ++ (go [] xs 0)
    where
        go taken left count
           | left == [] = [taken]
           | otherwise = (++) b1 b2
           where
               b1 = (go ((head left):taken) (tail left) (count+1))
               b2 = (go taken (tail left) count)