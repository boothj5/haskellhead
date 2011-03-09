module Game where 

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
                        
numDecksRequired :: (Integral t, Integral a) => a -> a -> t
numDecksRequired cs ps = div52 (fromIntegral $ total cs ps) + remDeck (total cs ps)
    where div52 n   = truncate $ n / 52
          remDeck n = if n `mod` 52 > 0 then 1 else 0
          total n m = n * m * 3
          
newDeck :: Deck
newDeck = [Card rank suit | suit <- [Hearts, Clubs, Diamonds, Spades], rank <- [Two .. Ace]]

newDeckWithEnoughCards :: Int -> Deck
newDeckWithEnoughCards 0 = []
newDeckWithEnoughCards 1 = newDeck
newDeckWithEnoughCards n = newDeck ++ newDeckWithEnoughCards (n-1)

createPlayers :: [String] -> PlayerCircle
createPlayers [] = []
createPlayers (x:[]) = [Player { name = x, hand = [], faceUp = [], faceDown = []}]
createPlayers (x:xs) = Player { name = x, hand = [], faceUp = [], faceDown = []} : createPlayers xs

getPlayer :: String -> PlayerCircle -> Maybe Player
getPlayer nameStr [] = Nothing
getPlayer nameStr ps = find (\p -> name p == nameStr) ps
                                   
addToNamedPlayersHand :: Player -> PlayerCircle -> [Card] -> PlayerCircle
addToNamedPlayersHand _ []     _   = []
addToNamedPlayersHand p1 (p2:ps) cs | p1 == p2    = addToHand p2 cs : ps
                                   | otherwise   = p2 : addToNamedPlayersHand p1 ps cs

addToNamedPlayersFaceUp :: Player -> PlayerCircle -> Card -> PlayerCircle
addToNamedPlayersFaceUp _ []     _   = []
addToNamedPlayersFaceUp p1 (p2:ps) c | p1 == p2    = addToFaceUp p2 c : ps
                                     | otherwise   = p2 : addToNamedPlayersFaceUp p1 ps c

addToNamedPlayersFaceDown :: Player -> PlayerCircle -> Card -> PlayerCircle
addToNamedPlayersFaceDown _ []     _   = []
addToNamedPlayersFaceDown p1 (p2:ps) c | p1 == p2    = addToFaceDown p2 c : ps
                                       | otherwise   = p2 : addToNamedPlayersFaceDown p1 ps c

swapForNamedPlayer :: Player -> PlayerCircle -> Int -> Int -> PlayerCircle
swapForNamedPlayer p1 (p2:ps) h f | p1 == p2  = swapHandWithFaceUp p2 h f : ps
                                  | otherwise = p2 : swapForNamedPlayer p1 ps h f

charToBoolean :: String -> Bool
charToBoolean s | toUpper (head s) == 'Y'= True
                | otherwise              = False

indexesFromString :: String -> [Integer]
indexesFromString str = fmap (\x -> read x -1) (stringsFromString str)

stringsFromString :: String -> [String]
stringsFromString = splitBy (== ',')

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list = first : splitBy f (dropWhile f rest) where
  (first, rest) = break f list    

playerWithLowestCardFromList :: PlayerCircle -> Player
playerWithLowestCardFromList [] = error "No players"
playerWithLowestCardFromList (player:[]) = player
playerWithLowestCardFromList (player:rest) = playerWithLowestCard player (playerWithLowestCardFromList rest)

removeFromNamedPlayersHand :: Player -> PlayerCircle -> [Card] -> PlayerCircle
removeFromNamedPlayersHand _ [] _        = []
removeFromNamedPlayersHand _ ps []       = ps
removeFromNamedPlayersHand p1 (p2:ps) cs | p1 == p2  = removeFromHand p2 cs : ps
                                         | otherwise = p2 : removeFromNamedPlayersHand p1 ps cs
                                         

removeFromNamedPlayersFaceUp :: Player -> PlayerCircle -> [Card] -> PlayerCircle
removeFromNamedPlayersFaceUp _ [] _        = []
removeFromNamedPlayersFaceUp _ ps []       = ps
removeFromNamedPlayersFaceUp p1 (p2:ps) cs | p1 == p2  = removeFromFaceUp p2 cs : ps
                                           | otherwise = p2 : removeFromNamedPlayersFaceUp p1 ps cs

removeFromNamedPlayersFaceDown :: Player -> PlayerCircle -> [Card] -> PlayerCircle
removeFromNamedPlayersFaceDown _ [] _        = []
removeFromNamedPlayersFaceDown _ ps []       = ps
removeFromNamedPlayersFaceDown p1 (p2:ps) cs | p1 == p2  = removeFromFaceDown p2 cs : ps
                                             | otherwise = p2 : removeFromNamedPlayersFaceDown p1 ps cs

nextTurn :: [a] -> [a]
nextTurn [] = []
nextTurn (p:[]) = [p]
nextTurn (p:ps) = ps ++ [p]

getCurrentPlayer :: PlayerCircle -> Maybe Player
getCurrentPlayer [] = Nothing
getCurrentPlayer (p:_) = Just p

moveToNextPlayerWithCards :: PlayerCircle -> PlayerCircle
moveToNextPlayerWithCards [] = []
moveToNextPlayerWithCards ps = if hasCards $ fromJust (getCurrentPlayer movedToNext) 
                                  then movedToNext 
                                  else moveToNextPlayerWithCards movedToNext
    where movedToNext = nextTurn ps
          
          
makeCurrentPlayer :: (Eq a) => a -> [a] -> [a]
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
                        
takeCardsFromPlayer :: Player -> [Player] -> [Card] -> [Player]
takeCardsFromPlayer p ps cs | hasCardsInHand p = removeFromNamedPlayersHand p ps cs
                            | hasCardsInFaceUp p = removeFromNamedPlayersFaceUp p ps cs
                            | otherwise = removeFromNamedPlayersFaceDown p ps cs                        
                        