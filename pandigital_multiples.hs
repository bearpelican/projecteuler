import Data.List

concatProduct1 x y = (concatProduct x [1..y], x, y)
concatProduct n ls = concat $ map (show . (*n)) ls

isPandigital (ls,_,_) = (length ls) == 9 && (sort ls) == "123456789"

filter (isPandigital) [ concatProduct1 x y | x <- [1..20000], y <- [1..20] ]
