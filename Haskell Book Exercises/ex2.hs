-- Basic Datatypes
--

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood Blah = Woot

answer x = do
  if x == answerToEveryThing
     then 
       putStrLn "You are a man of culture!!"
  else
     putStrLn "Boo..."
  where answerToEveryThing = 42

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == (reverse x)

myAbs :: Integer -> Integer
myAbs x = do 
  if x < 0
    then x * (-1)
  else x

zipRev :: (a, b) -> (c, d) -> ((b, d), (a, c))
zipRev p1 p2 = ((snd p1, snd p2), (fst p1, fst p2))


-- Correction
-- 

x = (+)
f xs = x w 1 where w = length xs 

iD x = x

f2 (a,b) = a

