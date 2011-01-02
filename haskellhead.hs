ranks = ["TWO", "THREE", "FOUR", "FIVE", "SIX", "SEVEN", "EIGHT", "NINE", "TEN", "JACK", "QUEEN", "KING", "ACE"]
suits = ["HEARTS", "CLUBS", "DIAMONDS", "SPADES"]

deck = [rank ++ " of " ++ suit | suit <- suits, rank <- ranks]
