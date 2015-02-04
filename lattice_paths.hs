latticePaths n = latticePaths' n (0, 0)

latticePaths' :: Int -> (Int, Int) -> Int
latticePaths' n (x, y) 
    | x == n && y == n = 1
    | x > n || y > n = 0
    | otherwise = (latticePaths' n (x+1, y)) + (latticePaths' n (x, y+1))

factorial n = product [1..n]
choose n k = factorial n `div` (factorial k * factorial (n-k))

lP n = choose (n+n) n
