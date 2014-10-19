coins :: [Integer]
coins = [200, 100, 50, 20, 10, 5, 2, 1]

change :: Integer -> Integer
change n = changeHelper n coins

changeHelper :: Integer -> [Integer] -> Integer
changeHelper 0 _ = 1
changeHelper _ [] = 0
changeHelper amount all@(x:xs)
      | amount < 0 = 0
      | otherwise = (changeHelper (amount - x) all) + (changeHelper amount xs)
