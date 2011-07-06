-- | Module for representing players and the functions that may be performed in them
module HumanPlayer 
( HumanPlayer(HumanPlayer, name, hand, faceUp, faceDown)
) where 

import Data.Maybe
import Data.List
import Card
import Player

data HumanPlayer = HumanPlayer { 
                name         :: String
               ,hand         :: Hand
               ,faceUp       :: Hand
               ,faceDown     :: Hand }

instance Show HumanPlayer where
    show p =
            "-----------------------------------------\n" ++ 
            "PLAYER:" ++ name p ++ "\n" ++
            "-----------------------------------------\n" ++ 
            "HAND:      " ++ showHand (hand p) False ++ "\n" ++
            "FACE UP:   " ++ showHand (faceUp p) False ++ "\n" ++
            "FACE DOWN: " ++ showHand (faceDown p) True ++ "\n"

instance Eq HumanPlayer where
    p1 == p2 = name p1 == name p2

instance Player HumanPlayer where

    getName p = name p
    
    getHand p = hand p
    
    getFaceUp p = faceUp p
    
    getFaceDown p = faceDown p
    
    getCard p n 
        | hasCardsInHand p   = hand p !! fromIntegral n
        | hasCardsInFaceUp p = faceUp p !! fromIntegral n
        | otherwise          = faceDown p !! fromIntegral n

    getCards p = map (getCard p) 

    hasCards p = hasCardsInHand p || hasCardsInFaceUp p || hasCardsInFaceDown p

    hasCardsInHand p = not . null . hand $ p

    hasCardsInFaceUp p = not . null . faceUp $ p

    hasCardsInFaceDown p = not . null . faceDown $ p

    playingFromFaceDown p = not (hasCardsInHand p) && not (hasCardsInFaceUp p)

    addToHand p cs = 
        HumanPlayer { name     = name p
                    , hand     = sortBy compareCardsSpecialHighest (cs ++ hand p)
                    , faceUp   = faceUp p
                    , faceDown = faceDown p }

    addToFaceUp p c = 
        HumanPlayer { name     = name p
                    , hand     = hand p
                    , faceUp   = c : faceUp p
                    , faceDown = faceDown p }

    addToFaceDown p c = 
        HumanPlayer { name     = name p
                    , hand     = hand p
                    , faceUp   = faceUp p
                    , faceDown = c : faceDown p }

    swapHandWithFaceUp p h f = 
        HumanPlayer { name     = name p
                    , hand     = sortBy compareCardsSpecialHighest (replaceCardWithCard (hand p) handCard faceUpCard)
                    , faceUp   = replaceCardWithCard (faceUp p) faceUpCard handCard
                    , faceDown = faceDown p }
	  where handCard   = hand p !! h
	        faceUpCard = faceUp p !! f

    playerWithLowestCard p1 p2 = if min p1Min p2Min == p1Min then p1 else p2
      where p1Min = lowestCard p1
            p2Min = lowestCard p2

    lowestCard p = minimum $ filter (not . layOnAnythingCard) (hand p)

    lowestCards p = lowest : filter (ranksEqual lowest) handMinusLowest
      where playersHand     = hand p
	    lowest          = lowestCard p
            handMinusLowest = filter (\c -> lowest /= c) playersHand

    removeFromHand p [] = p
    removeFromHand p cs = 
        HumanPlayer { name     = name p
                    , hand     = removeCards cs (hand p)
                    , faceUp   = faceUp p
                    , faceDown = faceDown p }

    removeFromFaceUp p [] = p
    removeFromFaceUp p cs = 
        HumanPlayer { name     = name p
                    , hand     = hand p
                    , faceUp   = removeCards cs (faceUp p)
                    , faceDown = faceDown p }

    removeFromFaceDown p [] = p
    removeFromFaceDown p cs = 
        HumanPlayer { name     = name p
                    , hand     = hand p
                    , faceUp   = faceUp p
                    , faceDown = removeCards cs (faceDown p)  }

-- | Return a string represantation
showCardAndPos :: Card -> Hand -> String
showCardAndPos c cs = show c ++ "(" ++ show (fromJust (elemIndex c cs)+1) ++ ")"

-- | Return a string representation of the players
-- If the boolean is true, we don't show the face down cards
showHand :: Hand -> Bool -> String
showHand [] _     = ""
showHand cs True  = foldl (\acc c -> acc ++ "****, ") "" cs
showHand cs False = foldl (\acc c -> acc ++ showCardAndPos c cs ++ ", ") "" cs

-- | Given a hand, replace on card in it with another one passed
replaceCardWithCard :: Hand -> Card -> Card -> Hand
replaceCardWithCard h old new = map (\c -> if old == c then new else c) h

-- | Remove cards from a hand
removeCards :: [Card] -> Hand -> Hand
removeCards cs = filter (`notElem` cs) 
