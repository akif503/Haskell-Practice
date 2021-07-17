-- Notes
-- This creates a new type call Thing with the given values, and adds the values to the Show typeclass  
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

data FailableDouble = Failure | OK Double deriving Show


type Name = String
type Age = Int
type FavThing = Thing

data Person = Person Name Age FavThing deriving Show

-- Recursive Data Types
-- 
-- Our Implementation of an Integer List
data IntList = Empty | Cons Int IntList

-- Example
--
myList :: IntList
myList = Cons 4 (Cons 5 (Cons 6 Empty))

-- Using a function to convert our custom list to built-in list 
convertMe :: IntList -> [Int]
convertMe Empty = []
convertMe (Cons x l) = x : (convertMe l)
