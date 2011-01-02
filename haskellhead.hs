ranks :: [[Char]]
ranks = ["TWO", "THREE", "FOUR", "FIVE", "SIX", "SEVEN", "EIGHT", "NINE", "TEN", "JACK", "QUEEN", "KING", "ACE"]

ranksNum :: [Integer]
ranksNum = [2,3..14]

suits :: [[Char]]
suits = ["HEARTS", "CLUBS", "DIAMONDS", "SPADES"]

showRank :: (Num a, Ord a) => a -> String
showRank a | a >= 2 && a <=10 = show a
showRank 11                   = "JACK"
showRank 12                   = "QUEEN"
showRank 13                   = "KING"
showRank 14                   = "ACE"
showRank _                    = error "Not a card"

deck :: [[Char]]
deck = [showRank rank ++ " of " ++ suit | suit <- suits, rank <- ranksNum]
