import Data.Char
sumDigit = sum $ map (digitToInt) (show n)
maximum [ sumDigit(x^y) | x <- [1..100], y <- [1..100] ]
