import Data.Char
import Data.List

reverseDigit :: Integer -> Integer
reverseDigit n = read ((reverse . show) n) ::Integer

reverseSum n = n + reverseDigit n

isValidReverse n = nLast /= 0 && ((odd nHead && even nLast) || (even nHead && odd nLast))
    where ls = show n
          nHead = digitToInt (head ls)
          nLast = digitToInt (last ls)


isReversible' n = all (odd . digitToInt) (show n)
isReversible n = isValidReverse n && isReversible' (reverseSum n)

