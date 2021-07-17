import Control.Monad (join)

-- Write bind (>>=) in terms of fmap & join
bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

-- Playing around
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = 
    xs >>= 
    (\x -> if even x
        then [x | n <- [1..x]]
        else [0])
    
--
-- Maybe Monad
--

data Cow = Cow {
    name :: String,
    age :: Int,
    weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String 
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n 
    | n >= 0 = Just n
    | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c = 
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
        then Nothing 
        else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = do
    n <- noEmpty name'
    a <- noNegative age'
    w <- noNegative weight'
    weightCheck (Cow n a w)

-- This(^) is equivalent to the following: 
-- mkSphericalCow name' age' weight' = notWeightChecked >>= weightCheck
--     where 
--         notWeightChecked =
--             Cow <$> noEmpty name'
--                 <*> noNegative age'
--                 <*> noNegative weight'

-- Also equivalent to:
-- mkSphericalCow name' age' weight' =
--     noEmpty name' >>=
--         \n -> noNegative age' >>=
--             \a -> noNegative weight' >>=
--                 \w -> weightCheck (Cow n a w)


--
-- Either Monad
-- 
type Founded = Int
type Coders = Int

data SoftwareShop = 
    Shop {
        founded :: Founded,
        programmers :: Coders
    } deriving (Eq, Show)

data FoundedError = 
      NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders 
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
    deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n 
    | n < 0 = Left $ NegativeYears n
    | n > 500 = Left $ TooManyYears n
    | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0 = Left $ NegativeCoders n
    | n > 5000 = Left $ TooManyCoders n
    | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded' <- validateFounded years
    programmers' <- validateCoders coders
    if programmers' > div founded' 10 
        then Left $
            TooManyCodersForYears
            founded' programmers'
        else Right $ Shop founded' programmers'