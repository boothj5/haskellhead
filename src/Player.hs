module Player 
( Player(Player)
, name
, hand
, faceUp
, faceDown
, hasCardsInHand
, hasCardsInFaceUp
, hasCards
, addToHand
, addToFaceUp
, addToFaceDown
, swapHandWithFaceUp
, playerWithLowestCard
, removeFromHand
, removeFromFaceUp
, removeFromFaceDown
, lowestCards
, playingFromFaceDown
, getCards
, getCard
) where 

import Data.Maybe
import Data.List
import Card
------------------------------------------------

--
-- Data types
--
type Hand = [Card]

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

------------------------------------------------
--
-- Player functions
--
showHand :: Hand -> Bool -> String
showHand [] _ = ""
showHand cs False = foldl (\acc card -> 
                            acc ++ show card ++ "(" ++ 
                            show (fromJust (elemIndex card cs)+1) ++ "), ") "" cs
showHand cs True = foldl (\acc card -> acc ++ "****, ") "" cs

getCard :: Player -> Integer -> Card
getCard p n | hasCardsInHand p = hand p !! fromIntegral n
            | hasCardsInFaceUp p = faceUp p !! fromIntegral n
            | otherwise = faceDown p !! fromIntegral n

getCards :: Player -> [Integer] -> [Card]
getCards p = map (getCard p) 

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

addToHand :: Player -> [Card] -> Player
addToHand p cs = Player { name     = name p
                        , hand     = sortBy compareCardsSpecialHighest (cs ++ hand p)
                        , faceUp   = faceUp p
                        , faceDown = faceDown p }

addToFaceUp :: Player -> Card -> Player
addToFaceUp p c = Player { name      = name p
                         , hand      = hand p
                         , faceUp    = c : faceUp p
                         , faceDown  = faceDown p }


addToFaceDown :: Player -> Card -> Player
addToFaceDown p c = Player { name        = name p
                           , hand        = hand p
                           , faceUp      = faceUp p
                           , faceDown    = c : faceDown p }


replaceCardWithCard :: Hand -> Card -> Card -> Hand
replaceCardWithCard h old new = map (\c -> if old == c then new else c) h

swapHandWithFaceUp :: Player -> Int -> Int -> Player
swapHandWithFaceUp p h f = Player { name     = name p
                                  , hand     = sortBy compareCardsSpecialHighest (replaceCardWithCard (hand p) handCard faceUpCard)
                                  , faceUp   = replaceCardWithCard (faceUp p) faceUpCard handCard
                                  , faceDown = faceDown p }
    where handCard   = hand p !! h
          faceUpCard = faceUp p !! f
          
playerWithLowestCard :: Player -> Player -> Player
playerWithLowestCard p1 p2 = if min p1Min p2Min == p1Min then p1 else p2
    where p1Min = lowestCard p1
          p2Min = lowestCard p2

lowestCard :: Player -> Card
lowestCard p = minimum $ filter (not . layOnAnythingCard) (hand p)

lowestCards :: Player -> [Card]
lowestCards p = lowest : filter (ranksAreEqual lowest) handMinusLowest
    where playersHand = hand p
          lowest = lowestCard p
          handMinusLowest = filter (\c -> lowest /= c) playersHand

removeCards :: [Card] -> Hand -> Hand
removeCards cs = filter (`notElem` cs) 

removeFromHand :: Player -> [Card] -> Player
removeFromHand p [] = p
removeFromHand p cs = Player { name = name p
                                     ,hand = removeCards cs (hand p)
                                     ,faceUp = faceUp p
                                     ,faceDown = faceDown p }

removeFromFaceUp :: Player -> [Card] -> Player
removeFromFaceUp p [] = p
removeFromFaceUp p cs = Player { name = name p
                                       ,hand = hand p
                                       ,faceUp = removeCards cs (faceUp p)
                                       ,faceDown = faceDown p }

removeFromFaceDown :: Player -> [Card] -> Player
removeFromFaceDown p [] = p
removeFromFaceDown p cs = Player { name = name p
                                         ,hand = hand p
                                         ,faceUp = faceUp p
                                         ,faceDown = removeCards cs (faceDown p)  }
                       