module Party where

import Data.Tree
import Employee
import Data.List

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons emp gl@(GL xs tf) = GL (emp:xs) (tf + (empFun emp))

-- Monoid for GuestList
-- In Employee.hs

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
    | f1 > f2 = gl1
    | otherwise = gl2

-- Exercise 2

-- Definition of Tree datatype 
-- data Tree a = Node {
--        rootLabel :: a,         -- label value
--        subForest :: [Tree a]   -- zero or more child trees
-- }


-- Folds the Tree datatype
treeFold :: (Eq a) => (a -> [b] -> b) -> b -> Tree a -> b
treeFold f z t@(Node a xs)
    | xs == [] = f a [z]
    | otherwise = f a (go f z xs)
    where
        go f z xs
            | xs == [] = []
            | otherwise = [treeFold f z $ head xs] ++ (go f z $ tail xs)


-- Just a test function to check if the treeFold is working properly
-- It memends the entire Tree
sumFun :: Tree Employee -> GuestList
sumFun = treeFold  (\e gls -> glCons e (convGLlist gls)) (mempty :: GuestList)
    where
        convGLlist = foldr (\gl1 gl2 -> gl1 <> gl2) (mempty :: GuestList) 


-- Exercise 3
-- Which one is more fun out of the 2 GuestLists of a pair
maxP :: (GuestList, GuestList) -> GuestList
maxP p = moreFun (fst p) (snd p)

-- If you pick the employee, then all other intermediate subordinates cannot be picked.
-- This is the logic behind the first argument of the pair.
-- The second argument is trickier, since the subordinates can be both picked or not.
-- We will take the maximum of the 2 arguments of the pair in this case.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp gls = ((foldPairedGL (pick True) snd), (foldPairedGL (pick False) maxP))
    where
        foldPairedGL base pos = (foldr (\p gl -> (pos p) <> gl) base gls)
        pick b = if b == True then (glCons emp mempty :: GuestList) else (mempty :: GuestList)


-- Exercise 4
-- Just see the function signature of the respective functions
maxFun :: Tree Employee -> GuestList
maxFun = maxP . treeFold nextLevel (ge, ge)
    where
        ge = mempty :: GuestList


-- Exercise 5
main :: IO ()
main = readFile "company.txt" >>= putStrLn . compute . read

-- Compute result
compute :: Tree Employee -> String
compute = formatGL . maxFun

-- Convert list of Employees into list of names
empLtoNames :: [Employee] -> [String]
empLtoNames = map empName

-- Sort a list of names by first name
-- We'll use quicksort
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

-- Converts the names into (First Name, Full Name) and sorts usign the first argument, then maps it back to fullname only
empNameSort :: [String] -> [String]
empNameSort = (map (\pname -> snd pname)) . quicksort . (map (\name -> (head $ words name, name)))

-- ConvertGL to format String
formatGL :: GuestList -> String
formatGL gl@(GL emps totalFun) = intercalate "\n" $ [formatTotalFun totalFun] ++ (empNameSort . empLtoNames $ emps)
    where 
        formatTotalFun fun = (++) "Total fun: " $ show fun




