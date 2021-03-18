-- Takes a string and splits it based on a "single" character
split :: Char -> [Char] -> [[Char]]
split c st = go st []
   where go st acc
          | st == "" = acc
          | otherwise = go (drop 1 $ dropWhile (/=c) st) $ acc ++ [takeWhile (/=c) st]


-- Splits the following poem based on \n
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immoral hand or eye\n"
fourthSen = "Could frame thy fearful\
             \ Symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- Splits words
myWords :: String -> [String] 
myWords = split ' ' st 

-- Splits sentences
myLines :: String -> [String] 
myLines st = split '\n' st


