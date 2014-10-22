module Fibonacci(fib) where

fib :: Int -> Int
fib = tailFib 0 1

tailFib :: Int -> Int -> Int -> Int
tailFib prev1 prev2 count
   | count == 0 = next
   | otherwise = tailFib next prev1 (count-1)
   where next = prev1 + prev2

underFour :: Int
underFour = sum $ filter (even) $ takeWhile (<4000000) (map fib [1..])
