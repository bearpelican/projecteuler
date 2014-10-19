import qualified Data.Map as Map
import Control.Applicative
import qualified Data.List as List
import Data.Function (on)
import qualified Data.Set as Set
import qualified Data.List.Split as Split

data Value = Two | Three | Four | Five | Six | Seven | 
             Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Show, Read)

valueList = [('2', Two), ('3', Three), ('4', Four), ('5', Five), ('6', Six) 
          ,('7', Seven), ('8', Eight), ('9', Nine), ('0', Ten), ('J', Jack)
          ,('Q', Queen), ('K', King), ('A', Ace)]

valueMap = Map.fromList valueList

data Suit = Club | Diamond | Heart | Spade deriving (Eq, Show, Read)

suitList = [('C', Club), ('D', Diamond), ('H', Heart), ('S', Spade)]

suitMap = Map.fromList suitList

type Card = (Value, Suit)

readCardStr :: String -> Maybe Card
readCardStr (_:v:s:[]) = readCard v s
readCardStr (v:s:[]) = readCard v s

readCard :: Char -> Char -> Maybe Card
readCard v s = (,) <$> cardValue v <*> cardSuit s
    where cardValue v = Map.lookup v valueMap
          cardSuit s = Map.lookup s suitMap



data Rank = High | Pair | TwoPair | ThreeKind | Straight |
            Flush | FullHouse | FourKind | StraightFlush deriving (Eq, Ord, Show)

type Hand = [Card]

type CardRank = (Rank, Hand)

-- Hand, Rank, High card
type BestHand = (Hand, CardRank, Card)

readHandStr :: String -> Hand
readHandStr str = map (readCardStr) cardSplit
    where cardSplit = Split.splitOn " " str

-- readHand :: Hand -> BestHand
-- readHand Hand




cardRank :: Hand -> CardRank
cardRank hand = cardRank' hand (rankFunctionList hand)

cardRank' :: Hand -> [(Rank, Maybe Hand)] -> CardRank
cardRank' hand [] = (High, [highCard hand])
cardRank' hand ((_, Nothing):xs) = cardRank' hand xs
cardRank' hand ((rank, Just h):xs) = (rank, h)

rankFunctionList :: Hand -> [(Rank, Maybe Hand)]
rankFunctionList hand = map (mapHand hand) [(StraightFlush, isStraightFlush), (FourKind, isFourKind), 
                                            (FullHouse, isFullHouse), (Flush, isFlush), (Straight, isStraight), 
                                            (ThreeKind, isThreeKind), (TwoPair, isTwoPair), (Pair, isPair)]
    where mapHand hand (rank, func) = (rank, func hand)






sortHand :: Hand -> Hand
sortHand hand = List.sortBy (compare `on` fst) hand

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

isSucc xs = and $ zipWith (<=) xs (drop 1 xs)


compareValue :: Card -> Card -> Ordering
compareValue (a,_) (b,_) = compare a b

cardEqual :: Card -> Card -> Bool
cardEqual (a,_) (b,_) = a == b


firstDuplicate :: Hand -> Hand
firstDuplicate xs = case duplicates  xs of
                      [] -> []
                      xs -> head xs

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
hasDupOrder x y list = and $ zipWith ($) [(hasLength x), (hasLength y)] list
    where hasLength n = (==n) . length

isFullHouse :: Hand -> Maybe Hand
isFullHouse hand
    | null list = Nothing
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
