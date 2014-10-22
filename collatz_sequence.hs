collats :: Int -> Int
collats x = collats' (x, 1)

collats' :: (Int, Int) -> Int
collats' (1, count) = count
collats' (x, count)
    | even x = collats' (div x 2, count+1)
    | otherwise = collats' (3*x + 1, count+1)
