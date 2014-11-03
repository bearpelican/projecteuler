import Data.List

isPermutedMultiple :: Integer -> Integer -> Bool
isPermutedMultiple multiplier x = (sortDig x) == (sortDig multiple)
    where multiple = x * multiplier
          sortDig = sort . show

isPM26 x = and $ map ($ x) (map isPermutedMultiple [2..6])
