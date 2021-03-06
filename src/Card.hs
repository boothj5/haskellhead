-- | Module for represneting cards and the functions on them
module Card 
( Card(Card, rank)
, Rank(..)
, Suit(..)
, compareCardsSpecialHighest
, layOnAnythingCard
, ranksEqual
, allRanksEqual
, burnRank
, missAGoRank
, invisibleRank
, resetRank
) where 

import Data.Char

-- | Possible ranks, (Ace is hight in this Enum)
data Rank = Two 
          | Three 
          | Four 
          | Five 
          | Six 
          | Seven 
          | Eight 
          | Nine 
          | Ten 
          | Jack 
          | Queen 
          | King 
          | Ace 
            deriving (Show, Eq, Ord, Enum)

-- | Possible Suits
data Suit = Hearts 
          | Clubs 
          | Diamonds 
          | Spades 
            deriving (Show, Eq, Ord)

-- | The card type, consits of Rank and Suit
data Card = Card { rank :: Rank
                 , suit :: Suit } deriving (Eq, Ord)

instance Show Card where
    show (Card rank suit) = 
        concat [ fmap toUpper (show rank)
               , " of "
               , fmap toUpper (show suit) ]

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

-- | Cards that can be layed on anything
layOnAnyThingRanks :: [Rank]
layOnAnyThingRanks = [ burnRank
                     , invisibleRank
                     , resetRank ]

-- | test whether a card may be layed on anything
layOnAnythingCard :: Card -> Bool
layOnAnythingCard c = rank c `elem` layOnAnyThingRanks

-- | test whether two cards have the same rank
ranksEqual :: Card -> Card -> Bool
ranksEqual c1 c2 = rank c1 == rank c2

-- | Test whether all cards in the list have the same rank
allRanksEqual :: [Card] -> Bool
allRanksEqual [] = False
allRanksEqual (c1:cs) = 
    foldl (\same c -> 
                if rank c /= rank c1 then False else same) 
          True 
          cs                        

-- | Compare two cards, special cards are considered higher than any other
-- but are considered equal to each other
compareCardsSpecialHighest :: Card -> Card -> Ordering
compareCardsSpecialHighest c1 c2 
    | layOnAnythingCard c1 && layOnAnythingCard c2       = EQ
    | layOnAnythingCard c1 && not (layOnAnythingCard c2) = GT
    | not (layOnAnythingCard c1) && layOnAnythingCard c2 = LT
    | otherwise                                          = compare c1 c2
