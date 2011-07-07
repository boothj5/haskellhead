-- | Module representing the game, and the functions that may be performed on it
module Game
( Game(Game, numPlayers, players, numCardsEach, deck, pile, burnt, lastMove)
, newDeckWithEnoughCards
, burn
, missAGo
, canMove
, inPlay
, validMove
) where 

import Data.Maybe
import Data.Char

import Card
import Player
import HumanPlayer
import PlayerCircle

type Pile = [Card]
type Deck = [Card]
type Burnt = [Card]

data Game = Game { numPlayers      :: Int
                 , players         :: PlayerCircle
                 , numCardsEach    :: Int
                 , deck            :: Deck
                 , pile            :: Pile
                 , burnt           :: Burnt
                 , lastMove        :: String
                 } 
                 
instance Show Game where
    show game  
        | null (lastMove game) = playersString game
        | otherwise            = concat [ playersString game
                                        , "\n"
                                        , lastMove game ]

playersString :: Game -> String    
playersString game = concat [ "\nPile : "
                            , show (pile game)
                            , "\n\n"
                            , show (length $ deck game)
                            , " remaining on deck\n\n"
                            , show (length $ burnt game)
                            , " burnt\n\n"
                            , showPlayers (players game) ]

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
          
                        