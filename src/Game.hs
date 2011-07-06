-- | Module representing the game, and the functions that may be performed on it
module Game
( Game(Game, numPlayers, players, numCardsEach, deck, pile, burnt, lastMove)
, newDeckWithEnoughCards
, createPlayers
, swapForPlayer
, removeCardsFromPlayer
, makeCurrentPlayer
, burn
, missAGo
, nextTurn
, addToPlayersHand
, removeFromPlayersFaceDown
, moveToNextPlayerWithCards
, getPlayer
, addToPlayersFaceUp
, addToPlayersFaceDown
, getShithead
, playerWithLowestCardFromCircle
, canMove
, inPlay
, validMove
) where 

import Data.Maybe
import Data.List
import Data.Char

import Card
import Player
import HumanPlayer

type Pile = [Card]
type Deck = [Card]
type PlayerCircle = [HumanPlayer]

data Game = Game { numPlayers      :: Int
                 , players         :: PlayerCircle
                 , numCardsEach    :: Int
                 , deck            :: Deck
                 , pile            :: Pile
                 , burnt           :: [Card]
                 , lastMove        :: String
                 } 
                 
instance Show Game where
    show game = if null (lastMove game) 
                   then playersString 
                   else playersString ++ "\n" ++ lastMove game
                    where playersString = "\nPile : " ++ show (pile game)
                            ++ "\n\n" ++ show (length $ deck game) ++ " remaining on deck" 
                            ++ "\n\n" ++ show (length $ burnt game) ++ " burnt"  
                            ++ "\n\n" ++ showPlayers (players game)

-- | Return a string representing all players in the game
showPlayers :: PlayerCircle -> String
showPlayers [] = ""
showPlayers (p:ps) = show p ++ "\n" ++ showPlayers ps

-- | Whether or not the given cards can be layed on the pile
validMove :: Card -> Pile -> Bool
validMove c  []       = True
validMove c1 (c2:cs)  
    | layOnAnythingCard c1     = True
    | rank c2 == invisibleRank = validMove c1 cs
    | rank c1 >= rank c2       = True
    | otherwise                = False

-- | Whether or not the player can actually move
canMove :: (Player a) => a -> Pile -> Bool
canMove p [] = True
canMove p cs 
    | hasCardsInHand p   = canMoveFromHand p cs 
    | hasCardsInFaceUp p = canMoveFromFaceUp p cs
    | otherwise          = False

-- | Whether or not the player can actually move fromn their hand
canMoveFromHand :: (Player a) => a -> Pile -> Bool
canMoveFromHand p [] = True
canMoveFromHand p cs = foldl (\can c -> (validMove c cs || can)) False (getHand p)

-- | Whether or not the player can actually move fromn their faceUp pile
canMoveFromFaceUp :: (Player a) => a -> Pile -> Bool
canMoveFromFaceUp p [] = True
canMoveFromFaceUp p cs = foldl (\can c -> (validMove c cs || can)) False (getFaceUp p)

-- | Whether or not the game is still in play
-- The game is in play if two or more players have cards
inPlay :: Game -> Bool
inPlay g = playersWithCards (players g) >= 2

-- | Returns how many players have cards
playersWithCards :: PlayerCircle -> Integer
playersWithCards [] = 0
playersWithCards (p:ps) 
    | hasCards p = 1 + playersWithCards ps
    | otherwise  = playersWithCards ps
                        
-- | Given the number of cards per hand, and the number of players, 
-- returns a new deck of cards, with enough cards to deal to all the players
newDeckWithEnoughCards :: Int -> Int -> Deck
newDeckWithEnoughCards cs ps = createDecks $ numDecksRequired cs ps
          
-- | Given the number of cards per hand, and the number of players,
-- returns how many decks of 52 cards are required
numDecksRequired :: (Integral t, Integral a) => a -> a -> t
numDecksRequired cs ps = div52 (fromIntegral $ total cs ps) + remDeck (total cs ps)
    where div52 n   = truncate $ n / 52
          remDeck n = if n `mod` 52 > 0 then 1 else 0
          total n m = n * m * 3

-- | Create the number of decks requried and returns one with them all in it
createDecks :: Int -> Deck
createDecks 0 = []
createDecks 1 = newDeck
createDecks n = newDeck ++ createDecks (n-1)

-- | Returns a new, unshuffled deck of cards
newDeck :: Deck
newDeck = [Card rank suit | suit <- [Hearts, Clubs, Diamonds, Spades], rank <- [Two .. Ace]]

-- | Given the players name, creates those players
createPlayers :: [String] -> PlayerCircle
createPlayers []     = []
createPlayers (x:[]) = [HumanPlayer { name = x, hand = [], faceUp = [], faceDown = []}]
createPlayers (x:xs) = HumanPlayer { name = x, hand = [], faceUp = [], faceDown = []} : createPlayers xs

-- | Get the player with the given name
getPlayer :: String -> PlayerCircle -> Maybe HumanPlayer
getPlayer _ [] = Nothing
getPlayer s ps = find (\p -> name p == s) ps
                                   
-- | Add some cards to the players hand
-- and returns a new player circle
addToPlayersHand :: HumanPlayer -> PlayerCircle -> [Card] -> PlayerCircle
addToPlayersHand _  []      _  = []
addToPlayersHand p1 (p2:ps) cs 
    | p1 == p2    = addToHand p2 cs : ps
    | otherwise   = p2 : addToPlayersHand p1 ps cs

-- | Add some cards to the players face up pile
-- and returns a new player circle
addToPlayersFaceUp :: HumanPlayer -> PlayerCircle -> Card -> PlayerCircle
addToPlayersFaceUp _  []      _ = []
addToPlayersFaceUp p1 (p2:ps) c 
    | p1 == p2    = addToFaceUp p2 c : ps
    | otherwise   = p2 : addToPlayersFaceUp p1 ps c

-- | Add some cards to the players face down pile
-- and returns a new player circle
addToPlayersFaceDown :: HumanPlayer -> PlayerCircle -> Card -> PlayerCircle
addToPlayersFaceDown _  []      _ = []
addToPlayersFaceDown p1 (p2:ps) c 
    | p1 == p2    = addToFaceDown p2 c : ps
    | otherwise   = p2 : addToPlayersFaceDown p1 ps c

-- | Swap cards between the players hand, and their face up pile
swapForPlayer :: HumanPlayer -> PlayerCircle -> Int -> Int -> PlayerCircle
swapForPlayer p1 (p2:ps) h f 
    | p1 == p2  = swapHandWithFaceUp p2 h f : ps
    | otherwise = p2 : swapForPlayer p1 ps h f

-- | Return the player with the lowest cards
playerWithLowestCardFromCircle :: PlayerCircle -> Maybe HumanPlayer
playerWithLowestCardFromCircle []     = Nothing
playerWithLowestCardFromCircle (p:[]) = Just p
playerWithLowestCardFromCircle (p:ps) = Just $ playerWithLowestCard p (fromJust $ playerWithLowestCardFromCircle ps)

-- | Remove a card from the players hand
-- and return a new list of players
removeFromPlayersHand :: HumanPlayer -> PlayerCircle -> [Card] -> PlayerCircle
removeFromPlayersHand _  []      _  = []
removeFromPlayersHand _  ps      [] = ps
removeFromPlayersHand p1 (p2:ps) cs 
    | p1 == p2  = removeFromHand p2 cs : ps
    | otherwise = p2 : removeFromPlayersHand p1 ps cs

-- | Remove a card from the players face up pile
-- and return a new list of players
removeFromPlayersFaceUp :: HumanPlayer -> PlayerCircle -> [Card] -> PlayerCircle
removeFromPlayersFaceUp _ [] _        = []
removeFromPlayersFaceUp _ ps []       = ps
removeFromPlayersFaceUp p1 (p2:ps) cs 
    | p1 == p2  = removeFromFaceUp p2 cs : ps
    | otherwise = p2 : removeFromPlayersFaceUp p1 ps cs

-- | Remove a card from the players face down pile
-- and return a new list of players
removeFromPlayersFaceDown :: HumanPlayer -> PlayerCircle -> [Card] -> PlayerCircle
removeFromPlayersFaceDown _ [] _        = []
removeFromPlayersFaceDown _ ps []       = ps
removeFromPlayersFaceDown p1 (p2:ps) cs 
    | p1 == p2  = removeFromFaceDown p2 cs : ps
    | otherwise = p2 : removeFromPlayersFaceDown p1 ps cs

-- | Move one player on, clockwise through the players
nextTurn :: [a] -> [a]
nextTurn [] = []
nextTurn (p:[]) = [p]
nextTurn (p:ps) = ps ++ [p]

-- | Move one player on, clockwise through the players
-- Skip those with no cards
moveToNextPlayerWithCards :: PlayerCircle -> PlayerCircle
moveToNextPlayerWithCards [] = []
moveToNextPlayerWithCards ps
    | hasCards (head $ nextTurn ps) = nextTurn ps 
    | otherwise                     = moveToNextPlayerWithCards $ nextTurn ps
          
-- | Make the named player the current player in the game
makeCurrentPlayer :: HumanPlayer -> PlayerCircle -> PlayerCircle
makeCurrentPlayer cp (p:ps) 
    | cp == p   = p:ps
    | otherwise = let newPs = nextTurn (p:ps) in makeCurrentPlayer cp newPs 
                                              

-- | Given a pile, burn it if there are burn cards on the top
burn :: Pile -> Pile
burn [] = []
burn l@(c1:[])          = if rank c1 == burnRank then [] else l
burn l@(c1:c2:[])       = if rank c1 == burnRank then [] else l
burn l@(c1:c2:c3:[])    = if rank c1 == burnRank then [] else l
burn l@(c1:c2:c3:c4:cs) = if (rank c1 == burnRank) || ranksSame then [] else l
    where ranksSame = allRanksEqual [c1,c2,c3,c4]
          
-- | Given a pile, return whether or not the next player should miss a go
missAGo :: Pile -> Bool
missAGo []    = False
missAGo (c:_) = rank c == missAGoRank
          
-- | Get the shithead from the list of players
getShithead :: PlayerCircle -> Maybe HumanPlayer
getShithead [] = Nothing
getShithead (p:ps) = if hasCards p then Just p else getShithead ps
                        
-- | Remove the cards from the players hand or face up pile
removeCardsFromPlayer :: HumanPlayer -> PlayerCircle -> [Card] -> PlayerCircle
removeCardsFromPlayer p ps cs 
    | hasCardsInHand p   = removeFromPlayersHand p ps cs
    | hasCardsInFaceUp p = removeFromPlayersFaceUp p ps cs
    | otherwise          = removeFromPlayersFaceDown p ps cs                        
                        