module Game
( Game(Game)
, numPlayers
, players
, numCardsEach
, deck
, pile
, burnt
, lastMove
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
------------------------------------------------

--
-- Data types
--
type Pile = [Card]
type Deck = [Card]
type PlayerCircle = [Player]

data Game = Game { numPlayers      :: Int
                                ,players         :: PlayerCircle
                                ,numCardsEach    :: Int
                                ,deck            :: Deck
                                ,pile            :: Pile
                                ,burnt           :: [Card]
                                ,lastMove        :: String
                               } 
instance Show Game where
    show game = if null (lastMove game) 
                   then playersString 
                   else playersString ++ "\n" ++ lastMove game
                    where playersString = "\nPile : " ++ show (pile game)
                            ++ "\n\n" ++ show (length $ deck game) ++ " remaining on deck" 
                            ++ "\n\n" ++ show (length $ burnt game) ++ " burnt"  
                            ++ "\n\n" ++ showPlayers (players game)

------------------------------------------------
--
-- game functions
--

showPlayers :: PlayerCircle -> String
showPlayers [] = ""
showPlayers (p:ps) = show p ++ "\n" ++ showPlayers ps

validMove :: Card -> Pile -> Bool
validMove c  []       = True
validMove c1 (c2:cs)  | layOnAnythingCard c1 = True
                      | rank c2 == invisibleRank = validMove c1 cs
                      | rank c1 >= rank c2  = True
                      | otherwise = False

canMove :: Player -> Pile -> Bool
canMove p [] = True
canMove p cs | hasCardsInHand p = canMoveFromHand p cs 
             | hasCardsInFaceUp p = canMoveFromFaceUp p cs
             | otherwise = False

canMoveFromHand :: Player -> Pile -> Bool
canMoveFromHand p [] = True
canMoveFromHand p cs = foldl (\can c -> (validMove c cs || can)) False (hand p)

canMoveFromFaceUp :: Player -> Pile -> Bool
canMoveFromFaceUp p [] = True
canMoveFromFaceUp p cs = foldl (\can c -> (validMove c cs || can)) False (faceUp p)

inPlay :: Game -> Bool
inPlay game = playersWithCards (players game) >= 2

playersWithCards :: PlayerCircle -> Integer
playersWithCards [] = 0
playersWithCards (p:ps) | hasCards p = 1 + playersWithCards ps
                        | otherwise  = playersWithCards ps
                        
newDeckWithEnoughCards :: Int -> Int -> Deck
newDeckWithEnoughCards cs ps = createDecks numDecks
    where numDecks = numDecksRequired cs ps
          
numDecksRequired :: (Integral t, Integral a) => a -> a -> t
numDecksRequired cs ps = div52 (fromIntegral $ total cs ps) + remDeck (total cs ps)
    where div52 n   = truncate $ n / 52
          remDeck n = if n `mod` 52 > 0 then 1 else 0
          total n m = n * m * 3

createDecks :: Int -> Deck
createDecks 0 = []
createDecks 1 = newDeck
createDecks n = newDeck ++ createDecks (n-1)

newDeck :: Deck
newDeck = [Card rank suit | suit <- [Hearts, Clubs, Diamonds, Spades], rank <- [Two .. Ace]]

createPlayers :: [String] -> PlayerCircle
createPlayers [] = []
createPlayers (x:[]) = [Player { name = x, hand = [], faceUp = [], faceDown = []}]
createPlayers (x:xs) = Player { name = x, hand = [], faceUp = [], faceDown = []} : createPlayers xs

getPlayer :: String -> PlayerCircle -> Maybe Player
getPlayer nameStr [] = Nothing
getPlayer nameStr ps = find (\p -> name p == nameStr) ps
                                   
addToPlayersHand :: Player -> PlayerCircle -> [Card] -> PlayerCircle
addToPlayersHand _ []     _   = []
addToPlayersHand p1 (p2:ps) cs | p1 == p2    = addToHand p2 cs : ps
                                   | otherwise   = p2 : addToPlayersHand p1 ps cs

addToPlayersFaceUp :: Player -> PlayerCircle -> Card -> PlayerCircle
addToPlayersFaceUp _ []     _   = []
addToPlayersFaceUp p1 (p2:ps) c | p1 == p2    = addToFaceUp p2 c : ps
                                     | otherwise   = p2 : addToPlayersFaceUp p1 ps c

addToPlayersFaceDown :: Player -> PlayerCircle -> Card -> PlayerCircle
addToPlayersFaceDown _ []     _   = []
addToPlayersFaceDown p1 (p2:ps) c | p1 == p2    = addToFaceDown p2 c : ps
                                       | otherwise   = p2 : addToPlayersFaceDown p1 ps c

swapForPlayer :: Player -> PlayerCircle -> Int -> Int -> PlayerCircle
swapForPlayer p1 (p2:ps) h f | p1 == p2  = swapHandWithFaceUp p2 h f : ps
                                  | otherwise = p2 : swapForPlayer p1 ps h f

playerWithLowestCardFromCircle :: PlayerCircle -> Player
playerWithLowestCardFromCircle [] = error "No players"
playerWithLowestCardFromCircle (player:[]) = player
playerWithLowestCardFromCircle (player:rest) = playerWithLowestCard player (playerWithLowestCardFromCircle rest)

removeFromPlayersHand :: Player -> PlayerCircle -> [Card] -> PlayerCircle
removeFromPlayersHand _ [] _        = []
removeFromPlayersHand _ ps []       = ps
removeFromPlayersHand p1 (p2:ps) cs | p1 == p2  = removeFromHand p2 cs : ps
                                         | otherwise = p2 : removeFromPlayersHand p1 ps cs
                                         

removeFromPlayersFaceUp :: Player -> PlayerCircle -> [Card] -> PlayerCircle
removeFromPlayersFaceUp _ [] _        = []
removeFromPlayersFaceUp _ ps []       = ps
removeFromPlayersFaceUp p1 (p2:ps) cs | p1 == p2  = removeFromFaceUp p2 cs : ps
                                           | otherwise = p2 : removeFromPlayersFaceUp p1 ps cs

removeFromPlayersFaceDown :: Player -> PlayerCircle -> [Card] -> PlayerCircle
removeFromPlayersFaceDown _ [] _        = []
removeFromPlayersFaceDown _ ps []       = ps
removeFromPlayersFaceDown p1 (p2:ps) cs | p1 == p2  = removeFromFaceDown p2 cs : ps
                                             | otherwise = p2 : removeFromPlayersFaceDown p1 ps cs

nextTurn :: [a] -> [a]
nextTurn [] = []
nextTurn (p:[]) = [p]
nextTurn (p:ps) = ps ++ [p]

moveToNextPlayerWithCards :: PlayerCircle -> PlayerCircle
moveToNextPlayerWithCards [] = []
moveToNextPlayerWithCards ps = if hasCards (head movedToNext) 
                                  then movedToNext 
                                  else moveToNextPlayerWithCards movedToNext
    where movedToNext = nextTurn ps
          
          
makeCurrentPlayer :: Player -> PlayerCircle -> PlayerCircle
makeCurrentPlayer cp (p:ps) | cp == p = p:ps
                            | otherwise = let newPs = nextTurn (p:ps) in makeCurrentPlayer cp newPs 
                                              

burn :: Pile -> Pile
burn [] = []
burn (c1:[])          = if rank c1 == burnRank then [] else [c1]
burn (c1:c2:[])       = if rank c1 == burnRank then [] else [c1,c2]
burn (c1:c2:c3:[])    = if rank c1 == burnRank then [] else [c1,c2,c3]
burn (c1:c2:c3:c4:cs) = if (rank c1 == burnRank) || ranksSame then [] else c1:c2:c3:c4:cs
    where ranksSame = (rank c1 == rank c2) && (rank c2 == rank c3) && (rank c3 == rank c4)
          
missAGo :: Pile -> Bool
missAGo [] = False
missAGo (c:_) = rank c == missAGoRank
          
getShithead :: PlayerCircle -> Maybe Player
getShithead [] = Nothing
getShithead (p:ps) = if hasCards p then Just p else getShithead ps
                        
removeCardsFromPlayer :: Player -> [Player] -> [Card] -> [Player]
removeCardsFromPlayer p ps cs | hasCardsInHand p = removeFromPlayersHand p ps cs
                              | hasCardsInFaceUp p = removeFromPlayersFaceUp p ps cs
                              | otherwise = removeFromPlayersFaceDown p ps cs                        
                        