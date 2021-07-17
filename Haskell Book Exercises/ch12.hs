{-
1. id :: a -> a 
- a is a nullary type 

2. r :: a -> f a
- a is a nullary type
- f is an unary type
-}

-- String Processing
-- replace the word 'the' with a
replace :: String -> String
replace sntc = go (words sntc)
    where 
        go (x:xs) 
            | xs == [] = (notThe x)
            | otherwise = (notThe x) ++ " " ++ (go $ xs)

notThe :: String -> String
notThe "the" = "a"
notThe xs = xs

-- Count the thes followed by a vowel-initial word
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel xs = go (words xs) 0
    where go (x:xs) count
           | xs == [] = count
           | otherwise = go xs getCount
            where
                getCount = case (validNext xs) of
                    False -> count
                    True -> if (check (x, head xs))
                            then count+1 else count
                validNext (x:xs)
                    | xs == [] = False
                    | otherwise = True
                check (x,y) = case x of
                    "the" -> if (head x `elem` "aeiou")
                            then True else False
                    _     -> False

-- Count the number of vowels
countVowels :: String -> Integer
countVowels (x:xs)
    | xs == [] = if (isVowel x) then 1 else 0
    | otherwise = case isVowel x of
        True -> (+) 1 $ countVowels xs
        False -> countVowels xs
    where
        isVowel x = elem x "aeiou"


data Nat = Zero | Succ Nat deriving (Eq, Show)

-- Natural to Int
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

-- Int to Natural
integerToNat :: Integer -> Maybe Nat
integerToNat z
    | z >= 0 = Just (go z)
    | otherwise = Nothing
    where 
        go z
           | z == 0 = Zero
           | otherwise = Succ $ go (z - 1)