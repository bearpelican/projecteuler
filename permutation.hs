import Data.Char
import Data.List

isPrime :: Integer -> Bool
isPrime k = null [ x | x <- [2..(floor . sqrt . fromInteger)k], k `mod`x  == 0]

permutations' :: Eq a => [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = [ x:ys | x <- xs, ys <- permutations' (delete x xs) ]

largestPrime :: [Integer] -> Maybe Integer
largestPrime [] = Nothing
largestPrime (x:xs) = (if isPrime x then Just x else largestPrime xs)

pandigitalPrime :: String -> Maybe Integer
pandigitalPrime x = largestPrime (perm x)
    where perm x = reverse (sort (toIntegerArray (permutations x)))
          toIntegerArray xs = map (read) xs :: [Integer]
