amicableSum = sum $ filter (\x -> x == amicable x && x /= divisorSum x) [1..10000]

divisorSum = sum . divisors

amicable n = sum $ divisors (sum $ divisors n)

divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]
