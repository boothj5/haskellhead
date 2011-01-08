-- ---------------
-- Card data types
-- ---------------

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
            deriving (Show, Eq, Ord, Enum)

data Suit = Hearts | Clubs | Diamonds | Spades deriving (Show, Eq)

data Card = Card Rank Suit deriving (Eq)

instance Show Card where
    show (Card rank suit) = show rank ++ " of " ++ show suit

deck :: [Card]
deck = [Card rank suit | suit <- [Hearts, Clubs, Diamonds, Spades], rank <- [Two .. Ace]]

