divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]

triangle1 = scanl (+) 1 [2..]
