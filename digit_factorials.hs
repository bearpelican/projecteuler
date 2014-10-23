import Data.Char

factorial n = product [2..n]

digit_factorials = filter (\x -> x == factorialSum x) [3..10000000]

factorialSum n = sum $ map (factorial . digitToInt) (show n)
