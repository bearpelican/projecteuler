module Main where
import Data.List
import Data.Maybe
import Control.Monad

readCard [r,s] = (parseRank r, parseSuit s)
 where parseSuit = translate "SHDC"
       parseRank = translate "23456789TJQKA"
       translate from x = fromJust $ findIndex (==x) from

solveHand hand = (handRank,tiebreak)
 where
 handRank
  | flush && straight   = 9
  | hasKinds 4          = 8
  | all hasKinds [2,3]  = 7
  | flush               = 6
  | straight            = 5
  | hasKinds 3          = 4
  | 1 < length (kind 2) = 3
  | hasKinds 2          = 2
  | otherwise           = 1
 tiebreak = kind =<< [4,3,2,1]
 hasKinds = not . null . kind
 kind n = map head $ filter ((n==).length) $ group ranks
 ranks  = reverse $ sort $ map fst hand
 flush  = 1 == length (nub (map snd hand))
 straight = length (kind 1) == 5 && 4 == head ranks - last ranks

gameLineToHands = splitAt 5 . map readCard . words
p1won (a,b) = solveHand a > solveHand b

main = do
    f <- readFile "poker.txt"
    let games = map gameLineToHands $ lines f
        wins = filter p1won games
    print $ length wins
