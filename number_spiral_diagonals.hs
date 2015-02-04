import Data.List
diagonal n = take 4 (iterate (subtract (n-1)) (n^2))
answer = sum . concat $ map diagonal [3,5..1001]
