import Data.List
import Control.Applicative

isPalindrome :: Integer -> Bool
isPalindrome w = (show w) == reverse (show w)
largestPalindromeProduct = maximum $ filter (isPalindrome) ((*) <$> [100..999] <*> [100..999])
