import Data.List

inits' :: [a] -> [[a]]
inits' = tail . init . inits

tails' :: [a] -> [[a]]
tails' = tail . init . tails

isPrime :: Integer -> Bool
isPrime k = null [ x | x <- [2..(floor . sqrt . fromInteger)k], k `mod`x  == 0]

toIntArray ::[String] -> [Integer]
toIntArray = map read

intTrunc :: Integer -> [Integer]
intTrunc n = (initArray n) ++ (tailArray n)
   where initArray = toIntArray . inits' . show
         tailArray = toIntArray . tails' . show

truncatablePrime :: Integer -> Bool
truncatablePrime x = isPrime x && (and $ map (isPrime) (intTrunc x))

truncatablePrimes = filter (truncatablePrime)
