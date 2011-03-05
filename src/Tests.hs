import Test.QuickCheck
import Text.Printf
import Game

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

-- check that changing turn returns same sized list
prop_nextTurnLengthLength l = (length $ nextTurn l) == (length l)
    where _ = l ::[Int]
          
-- check that changing turn puts first and end of list
prop_nextTurnTailPlusHead [] = True
prop_nextTurnTailPlusHead l = nextTurn l == (tail l) ++ (head l):[]
    where _ = l ::[Int]

tests = [("nextTurnLength/Length", quickCheck prop_nextTurnLengthLength)
        ,("nextTurn/TailPlusHead", quickCheck prop_nextTurnTailPlusHead) ]