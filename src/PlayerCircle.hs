module PlayerCircle 
( PlayerCircle
, showPlayers
, playersWithCards
, createPlayers
, swapForPlayer
, removeCardsFromPlayer
, makeCurrentPlayer
, nextTurn
, addToPlayersFaceDown
, addToPlayersFaceUp
, addToPlayersHand
, removeFromPlayersFaceDown
, moveToNextPlayerWithCards
, getPlayer
, getShithead
, playerWithLowestCardFromCircle
) where

import Data.List
import Data.Maybe

import Card
import Player
import HumanPlayer

type PlayerCircle = [HumanPlayer]

-- | Return a string representing all players in the game
showPlayers :: PlayerCircle -> String
showPlayers [] = ""
showPlayers (p:ps) = show p ++ "\n" ++ showPlayers ps

-- | Returns how many players have cards
playersWithCards :: PlayerCircle -> Integer
playersWithCards [] = 0
playersWithCards (p:ps) 
    | hasCards p = 1 + playersWithCards ps
    | otherwise  = playersWithCards ps

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
