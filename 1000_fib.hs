fibs = scanl (+) 0 (1:fibs)
thousandDigit = length $ takeWhile ((< 1000) . length . show) fibs
