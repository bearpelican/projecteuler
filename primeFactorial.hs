factorial x = [y | y <- [2..(quot x 2)], x `mod` y == 0]

primeFactorial :: Integer -> [Integer]
primeFactorial m = [x | x <- factorial m, isPrime (quot m x)]

isPrime :: Integer -> Bool
isPrime k = null [ x | x <- [2..(floor . sqrt . fromInteger)k], k `mod`x  == 0]

sumOfSquares :: [Integer] -> Integer
sumOfSquares = foldl (\acc x -> acc + x^2) 0

squareOfSum :: [Integer] -> Integer
squareOfSum x = (sum x) ^ 2

differenceOfSquares :: [Integer] -> Integer
differenceOfSquares x = squareOfSum x - sumOfSquares x
