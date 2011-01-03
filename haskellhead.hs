ranks :: [[Char]]
ranks = ["TWO", "THREE", "FOUR", "FIVE", "SIX", "SEVEN", "EIGHT", "NINE", "TEN", "JACK", "QUEEN", "KING", "ACE"]

ranksNum :: [Integer]
ranksNum = [2,3..14]

suits :: [[Char]]
suits = ["HEARTS", "CLUBS", "DIAMONDS", "SPADES"]

showRank :: (Num a, Ord a) => a -> String
showRank a | a >= 2 && a <=10 = show a
showRank 11                   = "JACK"
showRank 12                   = "QUEEN"
showRank 13                   = "KING"
showRank 14                   = "ACE"
showRank _                    = error "Not a card"

deck :: [[Char]]
deck = [showRank rank ++ " of " ++ suit | suit <- suits, rank <- ranksNum]

deckUsingNums :: [(Integer, [Char])]
deckUsingNums = [(rank, suit) | suit <- suits, rank <- ranksNum]

showCard :: (Num t, Ord t) => (t, [Char]) -> [Char]
showCard (a, b) = showRank a ++ " of " ++ b

showDeckAsCards a = map showCard a

-- ---------------
-- Card data types
-- ---------------

-- Very basic
data Rank = Rank String deriving (Show)
data Suit = Suit String deriving (Show)
data Card = Card (Rank, Suit) deriving (Show)
myCard = Card (Rank "THREE", Suit "HEARTS")

getRank :: Card -> Rank
getRank (Card (r, s)) = r

-- Using record syntax
data Card2 = Card2 {rank :: String, suit :: String} deriving (Show, Eq)
myCard2 = Card2 {rank="THREE", suit="HEARTS"}
myCard3 = Card2 {rank="THREE", suit="HEARTS"}
myCard4 = Card2 {rank="NINE", suit="CLUBS"}

-- Using enum for rank
data Rank2 = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
            deriving (Show, Eq, Ord, Enum)

data Suit2 = Hearts | Clubs | Diamonds | Spades deriving (Show, Eq)

data Card3 = Card3 (Rank2, Suit2) deriving (Show, Eq)

