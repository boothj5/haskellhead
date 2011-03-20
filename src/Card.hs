-- | Module for represneting cards and the functions that may be performed on them
module Card 
( Card(Card, rank)
, Rank(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
, Suit(Hearts, Clubs, Diamonds, Spades)
, compareCardsSpecialHighest
, layOnAnythingCard
, ranksAreEqual
, burnRank
, missAGoRank
, invisibleRank
, resetRank
, allRanksSame
) where 

import Data.Char

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
    deriving (Show, Eq, Ord, Enum)

data Suit = Hearts | Clubs | Diamonds | Spades deriving (Show, Eq, Ord)

data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Eq, Ord)

instance Show Card where
    show (Card rank suit) = fmap toUpper (show rank) ++ " of " ++ fmap toUpper (show suit)

-- | Card which will burn the pile
burnRank :: Rank
burnRank = Ten

-- | Card which skip the next player
missAGoRank :: Rank
missAGoRank = Eight

-- | When layed, doesn't have effect on last card
invisibleRank :: Rank
invisibleRank = Seven

-- | Brings pile back to this rank
resetRank :: Rank
resetRank = Two

-- | test whether a card may be layed on anything
layOnAnythingCard :: Card -> Bool
layOnAnythingCard c = rank c `elem` layOnAnyThingRanks

-- | test whether to cards have the same rank
ranksAreEqual :: Card -> Card -> Bool
ranksAreEqual c1 c2 = rank c1 == rank c2

-- | Compare two cards, special cards are considered higher than any other
-- but are considered equal to each other
compareCardsSpecialHighest :: Card -> Card -> Ordering
compareCardsSpecialHighest c1 c2 
    | layOnAnythingCard c1 && layOnAnythingCard c2 = EQ
    | layOnAnythingCard c1 && not (layOnAnythingCard c2) = GT
    | not (layOnAnythingCard c1) && layOnAnythingCard c2 = LT
    | otherwise = compare c1 c2
    
-- | Cards that can be layed on anything
layOnAnyThingRanks :: [Rank]
layOnAnyThingRanks = [burnRank, invisibleRank, resetRank]

-- | Test whether all cards in the list have the same rank
allRanksSame :: [Card] -> Bool
allRanksSame [] = False
allRanksSame (_:[]) = True
allRanksSame (c1:cs) = foldl (\same c -> if rank c /= rank c1 then False else same) True cs                        
