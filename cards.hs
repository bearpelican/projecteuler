import qualified Data.Map as Map
import Control.Applicative
import qualified Data.List as List
import Data.Function (on)
import qualified Data.Set as Set
import qualified Data.List.Split as Split
import Data.Monoid

import System.IO

main = do
  contents <- readFile "poker.txt"
  let hands = map (readPlayerHandStr) (lines contents)
  let winner = map (compareHand) hands
  let number = length $ filter (== GT) winner
  putStr (show number)
  putStr (show winner)

data Value = MIN | Two | Three | Four | Five | Six | Seven | 
             Eight | Nine | Ten | Jack | Queen | King | Ace | MAX deriving (Eq, Ord, Show, Enum)

valueList = [('2', Two), ('3', Three), ('4', Four), ('5', Five), ('6', Six) 
          ,('7', Seven), ('8', Eight), ('9', Nine), ('T', Ten), ('J', Jack)
          ,('Q', Queen), ('K', King), ('A', Ace)]

valueMap = Map.fromList valueList

data Suit = Club | Diamond | Heart | Spade deriving (Eq, Show, Read)

suitList = [('C', Club), ('D', Diamond), ('H', Heart), ('S', Spade)]

suitMap = Map.fromList suitList

type Card = (Value, Suit)

readCardStr :: String -> Maybe Card
readCardStr (v:s:[]) = readCard v s

readCard :: Char -> Char -> Maybe Card
readCard v s = (,) <$> cardValue v <*> cardSuit s
    where cardValue v = Map.lookup v valueMap
          cardSuit s = Map.lookup s suitMap



data Rank = High | Pair | TwoPair | ThreeKind | Straight |
            Flush | FullHouse | FourKind | StraightFlush deriving (Eq, Ord, Show)

type Hand = [Card]

type CardRank = (Rank, Hand)

-- Hand, Rank, Remainder
type BestHand = (CardRank, Hand)





mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

readPlayerHandStr :: String -> (Hand, Hand)
readPlayerHandStr str = mapTuple (readHandStr) (splitAt 5 cardSplit)
    where cardSplit = Split.splitOn " " str

readHandStr :: [String] -> Hand
readHandStr [] = []
readHandStr (x:xs) = case readCardStr x of
                        Nothing -> []
                        Just card -> card : (readHandStr xs)

-- readHand :: Hand -> BestHand
-- readHand Hand

compareHand :: (Hand, Hand) -> Ordering
compareHand (p1, p2) = compareBestHand (bestHand p1) (bestHand p2)

compareBestHand :: BestHand -> BestHand -> Ordering
compareBestHand (cr1, rm1) (cr2, rm2) = (compareCardRank cr1 cr2) `mappend` (compareHighCard rm1 rm2)

compareCardRank :: CardRank -> CardRank -> Ordering
compareCardRank  (r1, h1) (r2, h2) = (r1 `compare` r2) `mappend` (compareHighCard h1 h2)

compareHighCard :: Hand -> Hand -> Ordering
compareHighCard h1 h2 = compareValue (head h1) (head h2)

bestHand :: Hand -> BestHand
bestHand hand = (cr, remainder)
    where cr = cardRank hand
          remainder = hand List.\\ (snd cr)


cardRank :: Hand -> CardRank
cardRank hand = case cardRank' (rankFunctionList sortedHand) of
                  Nothing -> (High, [head sortedHand])
                  Just rank -> rank
    where sortedHand = sortHand hand

cardRank' :: [(Rank, Maybe Hand)] -> Maybe CardRank
cardRank' [] = Nothing
cardRank' ((_, Nothing):xs) = cardRank' xs
cardRank' ((rank, Just h):xs) = Just (rank, h)

rankFunctionList :: Hand -> [(Rank, Maybe Hand)]
rankFunctionList hand = map (mapHand hand) [(StraightFlush, isStraightFlush), (FourKind, isFourKind), 
                                            (FullHouse, isFullHouse), (Flush, isFlush), (Straight, isStraight), 
                                            (ThreeKind, isThreeKind), (TwoPair, isTwoPair), (Pair, isPair)]
    where mapHand hand (rank, func) = (rank, func hand)



sortHand :: Hand -> Hand
sortHand hand = sortByDescending (compare `on` fst) hand

sortByDescending cmp = List.sortBy (flip cmp)

values :: Hand -> [Value]
values = map (fst)

suits :: Hand -> [Suit]
suits = map (snd)

isStraightFlush :: Hand -> Maybe Hand
isStraightFlush hand = isFlush hand >>= isStraight

isFlush :: Hand -> Maybe Hand
isFlush [] = Nothing
isFlush hand
    | allTheSame (suits hand) = Just hand
    | otherwise = Nothing

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)


isStraight :: Hand -> Maybe Hand
isStraight [] = Nothing
isStraight hand
    | isSucc (values hand) = Just hand
    | otherwise = Nothing

isSucc xs = (order pred) || (order succ)
-- isSucc xs = (order prec)
    where order func = and $ zipWith ((==) . func) xs (tail xs)

compareValue :: Card -> Card -> Ordering
compareValue (a,_) (b,_) = compare a b

cardEqual :: Card -> Card -> Bool
cardEqual (a,_) (b,_) = a == b


firstDuplicate :: Hand -> Hand
firstDuplicate xs = case duplicates  xs of
                      [] -> []
                      xs -> head xs

-- use list group
duplicates :: Hand -> [Hand]
duplicates [] = []
duplicates (x:xs)
    | null dup = duplicates xs
    | otherwise = (x:dup) : (duplicates rem)
    where sep = List.span (cardEqual x) xs
          dup = fst sep
          rem = snd sep


isFourKind :: Hand -> Maybe Hand
isFourKind = isNKind 4

isThreeKind :: Hand -> Maybe Hand
isThreeKind = isNKind 3

isPair :: Hand -> Maybe Hand
isPair = isNKind 2

isNKind :: Int -> Hand -> Maybe Hand
isNKind n hand
    | (length duplicate) == n = Just duplicate
    | otherwise = Nothing
    where duplicate = firstDuplicate hand

hasDupOrder :: Int -> Int -> [[Card]] -> Bool
hasDupOrder x y list = hasOrder && ((length list) == 2)
    where hasLength n = (==n) . length
          hasOrder = and $ zipWith ($) [(hasLength x), (hasLength y)] list

isFullHouse :: Hand -> Maybe Hand
isFullHouse hand
    | hasOrder 2 3 = Just ((last list) ++ (head list))
    | hasOrder 3 2 = Just hand
    | otherwise = Nothing
    where list = duplicates hand
          hasOrder x y = hasDupOrder x y list

isTwoPair :: Hand -> Maybe Hand
isTwoPair hand
    | null list = Nothing
    | hasOrder 2 2 = Just (concat list)
    | otherwise = Nothing
    where list = duplicates hand
          hasOrder x y = hasDupOrder x y list

highCard :: Hand -> Card
highCard = head
