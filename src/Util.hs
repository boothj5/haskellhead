module Util
( charToBoolean
, indexesFromString
) where 

import Data.Char

charToBoolean :: String -> Bool
charToBoolean s | toUpper (head s) == 'Y'= True
                | otherwise              = False

indexesFromString :: String -> [Integer]
indexesFromString str = fmap (\x -> read x -1) (stringsFromString str)

stringsFromString :: String -> [String]
stringsFromString = splitBy (== ',')

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list = first : splitBy f (dropWhile f rest) 
    where (first, rest) = break f list  