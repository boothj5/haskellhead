module Game where 

import Data.Char
import Data.Maybe
import Data.List

------------------------------------------------

--
-- Data types
--
type Pile = [Card]
type Hand = [Card]
type Deck = [Card]
type PlayerCircle = [Player]

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
            deriving (Show, Eq, Ord, Enum)

data Suit = Hearts | Clubs | Diamonds | Spades deriving (Show, Eq, Ord)

data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Eq, Ord)

instance Show Card where
    show (Card rank suit) = fmap toUpper (show rank) ++ " of " ++ fmap toUpper (show suit)

data Player = Player { 
                name         :: String
               ,hand         :: Hand
               ,faceUp       :: Hand
               ,faceDown     :: Hand }

instance Show Player where
    show p =
            "-----------------------------------------\n" ++ 
            "PLAYER:" ++ name p ++ "\n" ++
            "-----------------------------------------\n" ++ 
            "HAND:      " ++ showHand (hand p) False ++ "\n" ++
            "FACE UP:   " ++ showHand (faceUp p) False ++ "\n" ++
            "FACE DOWN: " ++ showHand (faceDown p) True ++ "\n"

instance Eq Player where
    p1 == p2 = name p1 == name p2

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

showHand :: Hand -> Bool -> String
showHand [] _ = ""
showHand cs False = foldl (\acc card -> 
                            acc ++ show card ++ "(" ++ 
                            show (fromJust (elemIndex card cs)+1) ++ "), ") "" cs
showHand cs True = foldl (\acc card -> acc ++ "****, ") "" cs

burnRank :: Rank
burnRank = Ten

missAGoRank :: Rank
missAGoRank = Eight

invisibleRank :: Rank
invisibleRank = Seven

resetRank :: Rank
resetRank = Two

layOnAnyThingRanks :: [Rank]
layOnAnyThingRanks = [burnRank, invisibleRank, resetRank]

layOnAnythingCard :: Card -> Bool
layOnAnythingCard (Card rank suit) = rank `elem` layOnAnyThingRanks

ranksAreEqual :: Card -> Card -> Bool
ranksAreEqual (Card r1 _) (Card r2 _) = r1 == r2

validMove :: Card -> Pile -> Bool
validMove c              []                = True
validMove (Card Two   _) _                 = True
validMove (Card Seven _) _                 = True
validMove (Card Ten   _) _                 = True
validMove c1             (Card Seven _:cs) = validMove c1 cs
validMove (Card r1    _) (Card r2 _   :_)  | r1 >= r2  = True
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

getCard :: Player -> Integer -> Card
getCard p n | hasCardsInHand p = hand p !! fromIntegral n
            | hasCardsInFaceUp p = faceUp p !! fromIntegral n
            | otherwise = faceDown p !! fromIntegral n

getCards :: Player -> [Integer] -> [Card]
getCards p = map (getCard p) 

inPlay :: Game -> Bool
inPlay game = playersWithCards (players game) >= 2

playersWithCards :: PlayerCircle -> Integer
playersWithCards [] = 0
playersWithCards (p:ps) | hasCards p = 1 + playersWithCards ps
                        | otherwise  = playersWithCards ps
                        
hasCards :: Player -> Bool
hasCards player = hasCardsInHand player || hasCardsInFaceUp player || hasCardsInFaceDown player

hasCardsInHand :: Player -> Bool
hasCardsInHand player = length (hand player) > 0

hasCardsInFaceUp :: Player -> Bool
hasCardsInFaceUp player = length (faceUp player) > 0

hasCardsInFaceDown :: Player -> Bool
hasCardsInFaceDown player = length (faceDown player) > 0

playingFromFaceDown :: Player -> Bool
playingFromFaceDown p = not (hasCardsInHand p) && not (hasCardsInFaceUp p)

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

compareCardsSpecialHighest :: Card -> Card -> Ordering
compareCardsSpecialHighest c1 c2 
    | layOnAnythingCard c1 && layOnAnythingCard c2 = EQ
    | layOnAnythingCard c1 && not (layOnAnythingCard c2) = GT
    | not (layOnAnythingCard c1) && layOnAnythingCard c2 = LT
    | otherwise = compare c1 c2
    
addToPlayersHand :: Player -> [Card] -> Player
addToPlayersHand p cs = Player { name        = name p
                                ,hand        = sortBy compareCardsSpecialHighest (cs ++ hand p)
                                ,faceUp      = faceUp p
                                ,faceDown    = faceDown p }

addToPlayersFaceUp :: Player -> Card -> Player
addToPlayersFaceUp p c = Player { name      = name p
                                 ,hand      = hand p
                                 ,faceUp    = c : faceUp p
                                 ,faceDown  = faceDown p }


addToPlayersFaceDown :: Player -> Card -> Player
addToPlayersFaceDown p c = Player { name        = name p
                                   ,hand        = hand p
                                   ,faceUp      = faceUp p
                                   ,faceDown    = c : faceDown p }
                                   
addToNamedPlayersHand :: Player -> PlayerCircle -> [Card] -> PlayerCircle
addToNamedPlayersHand _ []     _   = []
addToNamedPlayersHand p1 (p2:ps) cs | p1 == p2    = addToPlayersHand p2 cs : ps
                                   | otherwise   = p2 : addToNamedPlayersHand p1 ps cs

addToNamedPlayersFaceUp :: Player -> PlayerCircle -> Card -> PlayerCircle
addToNamedPlayersFaceUp _ []     _   = []
addToNamedPlayersFaceUp p1 (p2:ps) c | p1 == p2    = addToPlayersFaceUp p2 c : ps
                                     | otherwise   = p2 : addToNamedPlayersFaceUp p1 ps c

addToNamedPlayersFaceDown :: Player -> PlayerCircle -> Card -> PlayerCircle
addToNamedPlayersFaceDown _ []     _   = []
addToNamedPlayersFaceDown p1 (p2:ps) c | p1 == p2    = addToPlayersFaceDown p2 c : ps
                                       | otherwise   = p2 : addToNamedPlayersFaceDown p1 ps c

swapHandWithFaceUp :: Player -> Int -> Int -> Player
swapHandWithFaceUp p h f = Player { name     = name p
                                   ,hand     = sort (map (\c -> if handCard == c then faceUpCard else c) (hand p))
                                   ,faceUp   = map (\c -> if faceUpCard == c then handCard else c) (faceUp p)
                                   ,faceDown = faceDown p }
    where handCard   = hand p !! h
          faceUpCard = faceUp p !! f
          
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

playerWithLowestCard :: Player -> Player -> Player
playerWithLowestCard p1 p2 = if min p1Min p2Min == p1Min then p1 else p2
    where p1Min = getLowestCard p1
          p2Min = getLowestCard p2

playerWithLowestCardFromList :: PlayerCircle -> Player
playerWithLowestCardFromList [] = error "No players"
playerWithLowestCardFromList (player:[]) = player
playerWithLowestCardFromList (player:rest) = playerWithLowestCard player (playerWithLowestCardFromList rest)

getLowestCard :: Player -> Card
getLowestCard p = minimum $ filter (not . layOnAnythingCard) (hand p)

getLowestCards :: Player -> [Card]
getLowestCards p = lowestCard : filter (ranksAreEqual lowestCard) handMinusLowest
    where playersHand = hand p
          lowestCard = getLowestCard p
          handMinusLowest = filter (\c -> lowestCard /= c) playersHand

removeFromPlayersHand :: Player -> [Card] -> Player
removeFromPlayersHand p [] = p
removeFromPlayersHand p cs = Player { name = name p
                                     ,hand = filter (`notElem` cs) $ hand p
                                     ,faceUp = faceUp p
                                     ,faceDown = faceDown p }

removeFromNamedPlayersHand :: Player -> PlayerCircle -> [Card] -> PlayerCircle
removeFromNamedPlayersHand _ [] _        = []
removeFromNamedPlayersHand _ ps []       = ps
removeFromNamedPlayersHand p1 (p2:ps) cs | p1 == p2  = removeFromPlayersHand p2 cs : ps
                                         | otherwise = p2 : removeFromNamedPlayersHand p1 ps cs
                                         

removeFromPlayersFaceUp :: Player -> [Card] -> Player
removeFromPlayersFaceUp p [] = p
removeFromPlayersFaceUp p cs = Player { name = name p
                                       ,hand = hand p
                                       ,faceUp = filter (`notElem` cs) $ faceUp p
                                       ,faceDown = faceDown p }

removeFromNamedPlayersFaceUp :: Player -> PlayerCircle -> [Card] -> PlayerCircle
removeFromNamedPlayersFaceUp _ [] _        = []
removeFromNamedPlayersFaceUp _ ps []       = ps
removeFromNamedPlayersFaceUp p1 (p2:ps) cs | p1 == p2  = removeFromPlayersFaceUp p2 cs : ps
                                           | otherwise = p2 : removeFromNamedPlayersFaceUp p1 ps cs

removeFromPlayersFaceDown :: Player -> [Card] -> Player
removeFromPlayersFaceDown p [] = p
removeFromPlayersFaceDown p cs = Player { name = name p
                                         ,hand = hand p
                                         ,faceUp = faceUp p
                                         ,faceDown = filter (`notElem` cs) $ faceDown p }

removeFromNamedPlayersFaceDown :: Player -> PlayerCircle -> [Card] -> PlayerCircle
removeFromNamedPlayersFaceDown _ [] _        = []
removeFromNamedPlayersFaceDown _ ps []       = ps
removeFromNamedPlayersFaceDown p1 (p2:ps) cs | p1 == p2  = removeFromPlayersFaceDown p2 cs : ps
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

sameRank :: [Card] -> Bool
sameRank [] = False
sameRank (_:[]) = True
sameRank (c1:cs) = foldl (\same c -> if rank c /= rank c1 then False else same) True cs
          
getShithead :: PlayerCircle -> Maybe Player
getShithead [] = Nothing
getShithead (p:ps) = if hasCards p then Just p else getShithead ps
                        
takeCardsFromPlayer :: Player -> [Player] -> [Card] -> [Player]
takeCardsFromPlayer p ps cs | hasCardsInHand p = removeFromNamedPlayersHand p ps cs
                            | hasCardsInFaceUp p = removeFromNamedPlayersFaceUp p ps cs
                            | otherwise = removeFromNamedPlayersFaceDown p ps cs                        
                        