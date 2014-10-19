import qualified Data.Map as Map
import Control.Applicative
import qualified Data.List as List
import Data.Function (on)

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

type CardRank = (Rank, Card)

-- Hand, Rank, High card
type BestHand = (Hand, CardRank, Card)

-- readHand :: Hand -> BestHand
-- readHand Hand

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

firstDuplicate :: (Eq a) => [a] -> [a]
firstDuplicate [] = []
firstDuplicate (x:[]) = [x]
firstDuplicate (x:y:xs) 
    | x == y = [x] ++ firstDuplicate (y:xs)
    | otherwise = firstDuplicate (y:xs)


isStraight :: Hand -> Maybe Hand
isStraight [] = Nothing
isStraight hand
    | isSucc (values hand) = Just hand
    | otherwise = Nothing

isSucc xs = and $ zipWith (<=) xs (drop 1 xs)


