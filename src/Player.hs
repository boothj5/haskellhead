module Player 
( Player
, Hand
, hasCardsInHand
, getCards
, getCard
, hasCards
, hasCardsInFaceUp
, hasCardsInFaceDown
, playingFromFaceDown
, addToHand
, addToFaceUp
, addToFaceDown
, swapHandWithFaceUp
, playerWithLowestCard
, lowestCard
, lowestCards
, removeFromHand
, removeFromFaceUp
, removeFromFaceDown
, getName
, getHand
, getFaceUp
, getFaceDown
) where

import Card

type Hand = [Card]

class Player a where
  
    -- | Return the players name
    getName :: a -> String
    
    -- | Return the players hand
    getHand :: a -> Hand
    
    -- | Return the players face up hand
    getFaceUp :: a -> Hand
    
    -- | Return the players face down hand
    getFaceDown :: a -> Hand

    -- | Get a card from the player
    getCard :: a -> Integer -> Card

    -- | Get a number of cards from the player
    getCards :: a -> [Integer] -> [Card]

    -- | Whether or not the player has any cards
    hasCards :: a -> Bool

    -- | Whether or not the player has any cards in their hand
    hasCardsInHand :: a -> Bool

    -- | Whether or not the player has any cards in their face up pile
    hasCardsInFaceUp :: a -> Bool

    -- | Whether or not the player has any cards in their face down pile
    hasCardsInFaceDown :: a -> Bool

    -- | Whether or not the player is playing from their face down pile
    playingFromFaceDown :: a -> Bool

    -- | Add some cards to the players hand
    addToHand :: a -> [Card] -> a

    -- | Add a card to the players face up hand
    addToFaceUp :: a -> Card -> a

    -- | Add a card to the players face down hand
    addToFaceDown :: a -> Card -> a

    -- | Swap a card in hand with one in the face up pile
    swapHandWithFaceUp :: a -> Int -> Int -> a

    -- | Return the player with the lowest cards
    playerWithLowestCard :: a -> a -> a

    -- | Return the players lowest card from their hand
    lowestCard :: a -> Card

    -- | Return the players lowest cards from their hand
    lowestCards :: a -> [Card]

    -- | Remove cards from a players hand
    removeFromHand :: a -> [Card] -> a

    -- | Remove cards from a players face up pile
    removeFromFaceUp :: a -> [Card] -> a

    -- | Remove cards from a players face down pile
    removeFromFaceDown :: a -> [Card] -> a  