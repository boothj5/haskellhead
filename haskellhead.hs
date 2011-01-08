data Player = Player { name :: String
                       , hand :: [Card]
                       , faceUp :: [Card]
                       , faceDown :: [Card] 
                     } 
instance Show Player where
    show (Player {name=n, hand=h, faceUp=u, faceDown=d}) = "\nplayer name: " ++ n
                                                           ++ "\nhand: " ++ show h
                                                           ++ "\nfaceUp: " ++ show u
                                                           ++ "\nfaceDown: " ++ show d

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
            deriving (Show, Eq, Ord, Enum)

data Suit = Hearts | Clubs | Diamonds | Spades deriving (Show, Eq)

data Card = Card Rank Suit deriving (Eq)
instance Show Card where
    show (Card rank suit) = show rank ++ " of " ++ show suit

deck :: [Card]
deck = [Card rank suit | suit <- [Hearts, Clubs, Diamonds, Spades], rank <- [Two .. Ace]]

jamesHand :: [Card]
jamesHand = [Card Three Diamonds, Card Two Hearts]

jamesFaceUp :: [Card]
jamesFaceUp = [Card Ace Spades, Card Four Hearts]

jamesFaceDown :: [Card]
jamesFaceDown = [Card Seven Clubs, Card Ten Diamonds]

james = Player {name="James", hand=jamesHand, faceUp=jamesFaceUp, faceDown=jamesFaceDown}

getGameInfo = do
    putStrLn "\nHow may players?"
    players <- getLine
    putStrLn ("ok, " ++ players ++ " players.")
    
showDeck = do
    putStrLn "\nDeck:"
    print deck

showPlayers = do 
    print james

main = do
    putStrLn "Haskell head"
    getGameInfo
    showDeck
    showPlayers

