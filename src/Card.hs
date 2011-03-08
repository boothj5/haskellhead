module Card 
( Card(Card)
, Rank(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
, Suit(Hearts, Clubs, Diamonds, Spades)
, compareCardsSpecialHighest
, layOnAnythingCard
, ranksAreEqual
, rank
, burnRank
, missAGoRank
, invisibleRank
, resetRank
, allRanksSame
) where 

import Data.Char
------------------------------------------------

--
-- Data types
--
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
    deriving (Show, Eq, Ord, Enum)

data Suit = Hearts | Clubs | Diamonds | Spades deriving (Show, Eq, Ord)

data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Eq, Ord)

instance Show Card where
    show (Card rank suit) = fmap toUpper (show rank) ++ " of " ++ fmap toUpper (show suit)

------------------------------------------------
--
-- Card functions
--
burnRank :: Rank
burnRank = Ten

missAGoRank :: Rank
missAGoRank = Eight

invisibleRank :: Rank
invisibleRank = Seven

resetRank :: Rank
resetRank = Two

layOnAnythingCard :: Card -> Bool
layOnAnythingCard c = rank c `elem` layOnAnyThingRanks

ranksAreEqual :: Card -> Card -> Bool
ranksAreEqual c1 c2 = rank c1 == rank c2

compareCardsSpecialHighest :: Card -> Card -> Ordering
compareCardsSpecialHighest c1 c2 
    | layOnAnythingCard c1 && layOnAnythingCard c2 = EQ
    | layOnAnythingCard c1 && not (layOnAnythingCard c2) = GT
    | not (layOnAnythingCard c1) && layOnAnythingCard c2 = LT
    | otherwise = compare c1 c2
    
layOnAnyThingRanks :: [Rank]
layOnAnyThingRanks = [burnRank, invisibleRank, resetRank]

allRanksSame :: [Card] -> Bool
allRanksSame [] = False
allRanksSame (_:[]) = True
allRanksSame (c1:cs) = foldl (\same c -> if rank c /= rank c1 then False else same) True cs                        
