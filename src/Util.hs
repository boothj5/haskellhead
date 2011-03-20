-- | A couple of basic helpers
module Util
( charToBoolean
, indexesFromString
) where 

import Data.Char

-- | If the string was 'y' or 'Y' returns true, false otherwise
charToBoolean :: String -> Bool
charToBoolean s | toUpper (head s) == 'Y'= True
                | otherwise              = False

-- | Turns the string, into a list of numbers
indexesFromString :: String -> [Integer]
indexesFromString str = fmap (\x -> read x -1) (stringsFromString str)

-- | Splits the string using the ','
stringsFromString :: String -> [String]
stringsFromString = splitBy (== ',')

-- | A basic string tokenizer
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list = first : splitBy f (dropWhile f rest) 
    where (first, rest) = break f list  