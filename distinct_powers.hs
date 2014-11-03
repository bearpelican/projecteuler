import Data.Set

powers = [ x ^ y | x <- [2..100], y <- [2..100] ]

distinctPowers = size $ fromList powers
