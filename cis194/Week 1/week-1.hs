-- Problem 1
-- Supporting Functions

extractDigits :: Integer -> [Integer]
extractDigits n 
  | n < 0 = []
  | n `div` 10 == 0 = n : []   
  | otherwise       = (n `mod` 10) : extractDigits(n `div` 10)  

extractDigitsRev :: Integer -> [Integer]
extractDigitsRev n = reverse (extractDigits n)

doubleEverySecond :: [Integer] -> [Integer]
doubleEverySecond x
  | null x        = []
  | null (tail x) = [head x]
  | otherwise     = (head x) : (2 * (head (tail x))) : doubleEverySecond (tail (tail x))

doubleEverySecondFromRight :: [Integer] -> [Integer]
doubleEverySecondFromRight x = reverse ( doubleEverySecond (reverse x) )

addEveryDigitOfList :: [Integer] -> Integer
addEveryDigitOfList (x:xs)
  | null xs   = sum (extractDigits x)
  | otherwise = sum (extractDigits x) + addEveryDigitOfList (xs)

checkSum :: [Integer] -> Integer
checkSum x = ( addEveryDigitOfList (doubleEverySecondFromRight x) ) `mod` 10 

-- Final Function
validate :: Integer -> Bool 
validate n = ( checkSum (extractDigitsRev n) ) == 0

-- Problem 2

type Peg = String
type Move = (Peg, Peg)

-- n (number of discs), cur, support, to (pegs)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi n a b c
  | n == 1 = (a,c) : []
  | otherwise = (hanoi (n-1) a c b) ++ [(a,c)] ++ (hanoi (n-1) b a c)

