module Game where 

import Data.Char
------------------------------------------------

--
-- Data types
--

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
            deriving (Show, Eq, Ord, Enum)
data Suit = Hearts | Clubs | Diamonds | Spades deriving (Show, Eq, Ord)
data Card = Card Rank Suit deriving (Eq, Ord)
instance Show Card where
    show (Card rank suit) = (fmap toUpper (show rank)) ++ " of " ++ (fmap toUpper (show suit))

data Player = Player { 
                name         :: String
               ,hand         :: [Card]
               ,faceUp       :: [Card]
               ,faceDown     :: [Card] }
instance Show Player where
    show (Player {name=n, hand=h, faceUp=u, faceDown=d}) = "\nplayer name: " ++ n
                                                           ++ "\nhand: " ++ show h
                                                           ++ "\nfaceUp: " ++ show u
                                                           ++ "\nfaceDown: " ++ show d
instance Eq Player where
    p1 == p2 = (name p1) == (name p2)

-- Used to represent the state of the game
data GameDetails = GameDetails { numPlayers      :: Int
                                ,players         :: [Player]
                                ,numCardsEach    :: Int
                                ,deck            :: [Card]
                                ,pile            :: [Card]
                               } 
instance Show GameDetails where
    show GameDetails { numPlayers   = n
                      ,players      = p
                      ,numCardsEach = c
                      ,deck         = d 
                      ,pile         = pile } =  "\nGame Details: " 
                          ++ "\nPlayers: " ++ show n
                          ++ "\nCards Each: " ++ show c
                          ++ "\n"
                          ++ "\nPlayers details: " ++ show p
                          ++ "\n" 
                          ++ "\nDeck : " ++ show d
                          ++ "\nPile : " ++ show pile


------------------------------------------------
--
-- game functions
--

-- determine the number of decks of cards required in game, 
-- given number of players and number of cards per hand
burnRank :: Rank
burnRank = Ten

missAGoRank :: Rank
missAGoRank = Eight

invisibleRank :: Rank
invisibleRank = Seven

resetRank :: Rank
resetRank = Two

specialRanks :: [Rank]
specialRanks = [burnRank, missAGoRank, invisibleRank, resetRank]

specialCard :: Card -> Bool
specialCard (Card rank suit) = rank `elem` specialRanks

equalsRank :: Card -> Card -> Bool
equalsRank (Card r1 _) (Card r2 _) = r1 == r2

numDecksRequired :: (Integral t, Integral a) => a -> a -> t
numDecksRequired cs ps = ( div52 $ fromIntegral $ total cs ps ) + ( remDeck $ total cs ps )
    where div52 n   = truncate $ n / 52
          remDeck n = if n `mod` 52 > 0 then 1 else 0
          total n m = n * m * 3
          
-- return a new unshuffled deck of cards
newDeck :: [Card]
newDeck = [Card rank suit | suit <- [Hearts, Clubs, Diamonds, Spades], rank <- [Two .. Ace]]

-- returns a number of decks as one, i.e. with two decks, 
-- every card will be represented twice
newDeckWithEnoughCards :: Int -> [Card]
newDeckWithEnoughCards 0 = []
newDeckWithEnoughCards 1 = newDeck
newDeckWithEnoughCards n = newDeck ++ (newDeckWithEnoughCards $ n-1)

-- Given a list of names will return a list of players with those names
createPlayers :: [String] -> [Player]
createPlayers [] = []
createPlayers (x:[]) = ( Player { name = x, hand = [], faceUp = [], faceDown = []} ) : []
createPlayers (x:xs) = ( Player { name = x, hand = [], faceUp = [], faceDown = []} ) : createPlayers xs

-- Given a player and a card, will return a new player, 
-- with every thing the same but the Card added to one of their hands
addToPlayersHand :: Player -> Card -> Player
addToPlayersHand p c = Player { name        = ( name p )
                               ,hand        = ( c : (hand p) )
                               ,faceUp      = ( faceUp p )
                               ,faceDown    = ( faceDown p ) }

addToPlayersFaceUp :: Player -> Card -> Player
addToPlayersFaceUp p c = Player { name      = ( name p )
                                 ,hand      = ( hand p )
                                 ,faceUp    = ( c : (faceUp p) )
                                 ,faceDown  = ( faceDown p ) }


addToPlayersFaceDown :: Player -> Card -> Player
addToPlayersFaceDown p c = Player { name        = ( name p )
                                   ,hand        = ( hand p )
                                   ,faceUp      = ( faceUp p )
                                   ,faceDown    = ( c : (faceDown p) ) }
                                   
-- Given a player, a list of players, and a card, returns
-- a list of players with everything the same but the card
-- added to one of the players hands, whos name matches that of the player 
-- passed in
addToNamedPlayersHand :: Player -> [Player] -> Card -> [Player]
addToNamedPlayersHand _ []     _   = []
addToNamedPlayersHand p1 (p2:ps) c | p1 == p2    = (addToPlayersHand p2 c) : ps
                                   | otherwise   = p2 : (addToNamedPlayersHand p1 ps c)

addToNamedPlayersFaceUp :: Player -> [Player] -> Card -> [Player]
addToNamedPlayersFaceUp _ []     _   = []
addToNamedPlayersFaceUp p1 (p2:ps) c | p1 == p2    = (addToPlayersFaceUp p2 c) : ps
                                     | otherwise   = p2 : (addToNamedPlayersFaceUp p1 ps c)

addToNamedPlayersFaceDown :: Player -> [Player] -> Card -> [Player]
addToNamedPlayersFaceDown _ []     _   = []
addToNamedPlayersFaceDown p1 (p2:ps) c | p1 == p2    = (addToPlayersFaceDown p2 c) : ps
                                       | otherwise   = p2 : (addToNamedPlayersFaceDown p1 ps c)

swapHandWithFaceUp :: Player -> Int -> Int -> Player
swapHandWithFaceUp p h f = Player { name     = ( name p )
                                   ,hand     = (map (\c -> if (handCard == c) then faceUpCard else c) (hand p))
                                   ,faceUp   = (map (\c -> if (faceUpCard == c) then handCard else c) (faceUp p))
                                   ,faceDown = ( faceDown p ) }
    where handCard   = (hand p) !! h
          faceUpCard = (faceUp p) !! f
          
swapForNamedPlayer :: Player -> [Player] -> Int -> Int -> [Player]
swapForNamedPlayer p1 (p2:ps) h f | p1 == p2  = (swapHandWithFaceUp p2 h f) : ps
                                  | otherwise = p2 : (swapForNamedPlayer p1 ps h f)

charToBoolean :: String -> Bool
charToBoolean s | (toUpper $ s !! 0) == 'Y'= True
        | otherwise                = False
        
playerWithLowestCard :: Player -> Player -> Player
playerWithLowestCard p1 p2 = if ((min p1Min p2Min) == p1Min) then p1 else p2
    where p1Min = getLowestCard p1
          p2Min = getLowestCard p2

playerWithLowestCardFromList :: [Player] -> Player
playerWithLowestCardFromList [] = error "No players"
playerWithLowestCardFromList (player:[]) = player
playerWithLowestCardFromList (player:rest) = playerWithLowestCard player (playerWithLowestCardFromList rest)

getLowestCard :: Player -> Card
getLowestCard p = minimum $ filter (\c -> not $ specialCard c) (hand p)

getLowestCards :: Player -> [Card]
getLowestCards p = lowestCard : filter (\c -> equalsRank lowestCard c) handMinusLowest
    where playersHand = hand p
          lowestCard = getLowestCard p
          handMinusLowest = filter (\c -> lowestCard /= c) playersHand

removeFromPlayersHand :: Player -> [Card] -> Player
removeFromPlayersHand p [] = p
removeFromPlayersHand p cs = Player { name = ( name p )
                                     ,hand = ( filter (\c -> c `notElem` cs) $ hand p )
                                     ,faceUp = ( faceUp p )
                                     ,faceDown = ( faceDown p ) }

removeFromNamedPlayersHand :: Player -> [Player] -> [Card] -> [Player]
removeFromNamedPlayersHand _ [] _        = []
removeFromNamedPlayersHand _ ps []       = ps
removeFromNamedPlayersHand p1 (p2:ps) cs | p1 == p2  = (removeFromPlayersHand p2 cs) : ps
                                         | otherwise = p2 : (removeFromNamedPlayersHand p1 ps cs)
                                         
nextTurn :: [a] -> [a]
nextTurn [] = []
nextTurn (p:[]) = (p:[])
nextTurn (p:ps) = ps ++ p:[]

makeCurrentPlayer :: (Eq a) => a -> [a] -> [a]
makeCurrentPlayer cp (p:ps) | cp == p = p:ps
                            | otherwise = let newPs = nextTurn (p:ps) in makeCurrentPlayer cp newPs 