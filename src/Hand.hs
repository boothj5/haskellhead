module Hand 
( Hand
, showHand
, replaceCardWithCard
, removeCards
) where

import Data.Maybe
import Data.List

import Card

type Hand = [Card]

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
