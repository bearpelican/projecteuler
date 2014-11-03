nCr :: Integer -> Integer -> Integer
nCr n r = (fact n) `div` ((fact r) * fact (n-r))
    where fact x = product [1..x]

nCrTo100 = filter (>1000000) $ nCr <$> [1..100] <*> [1..100]
