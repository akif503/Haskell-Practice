{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (partition)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)


-- Exercise 2
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  as <- getDieRolls curAttackers
  ds <- getDieRolls curDefenders
  return $ battleRes (attackers bf) (defenders bf) as ds
  where
    curAttackers = noOfAttackers bf
    curDefenders = noOfDefenders bf

battleRes ca cd as ds = Battlefield (ca - atkl) (cd - defl)
  where
    (atkl, defl) = soldiersLost $ map (winnerOfEachRoll) $ zip (quickSort as) (quickSort ds)

noOfAttackers :: Battlefield -> Int
noOfAttackers bf = 
  case (currentArmy > 3) of
    True -> 3
    False -> case (currentArmy > 1) of
      True -> currentArmy - 1
      False -> 0
  where currentArmy = attackers bf

getDieRolls :: Int ->  Rand StdGen [Int]
getDieRolls noOfSoldiers =
  case noOfSoldiers <= 0 of
    True -> return [] 
    --False -> die >>= (\d -> (sortedDieRolls (noOfSoldiers - 1)) >>= return (\ds -> d:ds))
    False -> do
      d <- die
      ds <- getDieRolls (noOfSoldiers - 1)
      return ((unDV d):ds)

-- True: if the attacker wins
-- False: defender
winnerOfEachRoll :: (Int, Int) -> Bool
winnerOfEachRoll (a,d) = a > d

-- The first argument specifies the numbers of soldiers the attackers lost == number of die rolls defenders won
-- Same for the other argument
soldiersLost :: [Bool] -> (Int, Int)
soldiersLost xs = (dfndrs xs, atckrs xs)
  where
    atckrs = foldr (\x -> if x==True then (+1) else (+0)) 0
    dfndrs = foldr (\x -> if x==False then (+1) else (+0)) 0

noOfDefenders :: Battlefield -> Int
noOfDefenders bf = 
  case (currentArmy >= 2) of
    True -> 2
    False -> currentArmy
  where currentArmy = defenders bf


-- Sorts in descending order
quickSort :: (Ord a) => [a] -> [a]
quickSort []       = []
quickSort (x : xs) =
    let (lt, gt) = partition (>= x) xs
    in  (quickSort lt) ++ [x] ++ (quickSort gt)


-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf = 
  case (attackers bf) < 2 of 
    True -> return bf
    False -> case (defenders bf) == 0 of
      True -> return bf
      False -> battle bf >>= (\nextBF -> invade nextBF)


-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb bf = (sequence (map run (take 1000 $ repeat bf))) >>= return . (\x -> (fromIntegral x) / 1000)  . sum
  where
    run bf = do
      res <- invade bf
      return $ winner res

-- Given the war is settled, i.e. there is only 1 attacker left or there are 0 defenders
-- 1: Attacker, 0: Defender

winner :: Battlefield -> Int
winner bf = 
  case (defenders bf) == 0 of
    True -> 1
    False -> 0
