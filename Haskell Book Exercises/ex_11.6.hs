data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited 
        deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline 
             deriving (Eq, Show) 


myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- 1. The type of myCar is Vehicle


-- 2. 

isCar :: Vehicle -> Bool
isCar (Car _ _)  = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars [] = []
areCars (x:xs) = (isCar x):(areCars xs)

-- 3.
--
getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x
