import Numeric
import Data.Char


isDBPalindrome x = isPalindrome (show x) && isPalindrome (binaryStr x)

isPalindrome x = x == reverse x

binaryStr :: Integer -> String
binaryStr n = showIntAtBase 2 intToDigit n ""

dbPalindromes = filter (isDBPalindrome) [1..1000000]
