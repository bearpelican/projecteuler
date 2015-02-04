import Data.List

shift :: [a] -> [[a]]
shift n = init $ zipWith (++) (tails n) (inits n)

isPrime :: Integer -> Bool
isPrime k = null [ x | x <- [2..(floor . sqrt . fromInteger)k], k `mod`x  == 0]

toIntArray ::[String] -> [Integer]
toIntArray = map read

intShift :: Integer -> [Integer]
intShift = toIntArray . shift . show

isCircularPrime :: Integer -> Bool
isCircularPrime x = and $ map (isPrime) (intShift x)

circularPrimes = filter (isCircularPrime)
