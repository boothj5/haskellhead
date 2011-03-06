import Test.HUnit
import Game

-- test moveing through the players
initial = [1,2,3,4]
expected = [2,3,4,1]
testNextTurn = 
    TestCase (assertEqual 
        "Test next turn, " 
        expected (nextTurn initial))

circleList list 0 = list
circleList list n = circleList (nextTurn list) (n-1)
testCompleteCicle = 
    TestCase (assertEqual 
        "Test complete circle, "
        initial (circleList initial (length initial)))

cardA = Card Six Spades
cardB = Card Jack Diamonds
cardC = Card Ten Hearts
cardD = Card Two Spades

playerWithNoCards = 
        ( Player { name = "Mark", hand = [], faceUp = [], faceDown = []} )
playerWithNoCards2 = 
        ( Player { name = "Stevie", hand = [], faceUp = [], faceDown = []} )
playerWithFaceDownCardsOnly = 
        ( Player { name = "James", hand = [], faceUp = [], faceDown = [cardA]} )
payerWithFaceUpAndFaceDownCards = 
        ( Player { name = "Davo", hand = [], faceUp = [cardB], faceDown = [cardA]} )
payerWithCardsInAllHands = 
        ( Player { name = "Monkey", hand = [cardC], faceUp = [cardB], faceDown = [cardA]} )

twoPlayersWithCards = [playerWithFaceDownCardsOnly, payerWithCardsInAllHands, playerWithNoCards]
onlyOnePlayerWithCards = [playerWithFaceDownCardsOnly, playerWithNoCards, playerWithNoCards]

gameInPlay = GameDetails { numPlayers      = 3
                       ,players         = twoPlayersWithCards
                       ,numCardsEach    = 0
                       ,deck            = [] 
                       ,pile            = []
                       ,burnt            = []
                       ,lastMove        = "" }

gameNotInPlay = GameDetails { numPlayers      = 3
                       ,players         = onlyOnePlayerWithCards
                       ,numCardsEach    = 0
                       ,deck            = [] 
                       ,pile            = []
                       ,burnt            = []
                       ,lastMove        = "" }


testInPlayWhenTwoPlayersHaveCards = 
    TestCase (assertBool
        "Test in play when two players have cards"
        (inPlay gameInPlay)) 

testNotInPlayWhenOnePlayerHasCards = 
    TestCase (assertBool
        "Test not in play when one player has cards"
        (not $ inPlay gameNotInPlay)) 
                                        
testOneDeckRequired = 
    TestCase (assertEqual
        "Test correct number of decks when only one needed"
        1 (numDecksRequired 3 2))

testTwoDeckRequired = 
    TestCase (assertEqual
        "Test correct number of decks when two needed"
        2 (numDecksRequired 3 6))

testBurnEmptyReturnsEmpty =
    TestCase (assertEqual
        "Test burn empty pile returns empty pile"
        [] (burn []))

pile1 = [Card Three Diamonds]
testBurnOneNotTenDoesNotBurn =
    TestCase (assertEqual
        "Test burn one card not ten returns same"
        pile1 (burn pile1))
   
pile2 = [Card Six Hearts, Card Two Diamonds, Card Ace Spades]
testBurnTHreeNotTenDoesNotBurn =
    TestCase (assertEqual
        "Test burn three cards no ten return same"
        pile2 (burn pile2))
   
pile3 = [Card Ten Hearts]
testBurnOnlyTenReturnsEmpty =
    TestCase (assertEqual
        "Test burn only ten returns empty"
        [] (burn pile3))

pile4 = [Card Ten Hearts, Card Ace Diamonds, Card Queen Hearts]
testBurnTenOnCardsReturnsEmpty =
    TestCase (assertEqual
        "Test burn when ten on other cards"
        [] (burn pile4))

pile5 = [Card Six Hearts, Card Six Spades, Card Six Clubs, Card Six Diamonds]
testBurnOnlyFourSixesReturnsEmpty =
    TestCase (assertEqual
        "Test burn just four sixes returns empty"
        [] (burn pile5))

pile6 = [Card Jack Spades, Card Jack Hearts, Card Jack Clubs, Card Jack Diamonds, Card Nine Spades]
testBurnFourJakcsOnOtherCardsReturnsEmpty =
    TestCase (assertEqual
        "Test burn four jacks on other cards returns empty"
        [] (burn pile6))

pile7 = [Card Ten Hearts] ++ pile1 ++ pile2 ++ [Card Nine Spades, Card Ace Hearts]
testBurnTenOnMoreThanFourCardsReturnsEmpty =
    TestCase (assertEqual
        "Test burn when ten on more than four cards"
        [] (burn pile7))



-- Suite
tests = TestList [TestLabel "NextTurn" testNextTurn
                , TestLabel "CompleteCicle " testCompleteCicle 
                , TestLabel "InPlayWhenTwoPlayersHaveCards" testInPlayWhenTwoPlayersHaveCards
                , TestLabel "NotInPlayWhenOnePlayerHasCards" testNotInPlayWhenOnePlayerHasCards
                , TestLabel "OneDeckRequired" testOneDeckRequired
                , TestLabel "TwoDeckRequired" testTwoDeckRequired
                , TestLabel "BurnEmptyReturnsEmpty" testBurnEmptyReturnsEmpty
                , TestLabel "BurnOneNotTenDoesNotBurn" testBurnOneNotTenDoesNotBurn
                , TestLabel "BurnTHreeNotTenDoesNotBurn" testBurnTHreeNotTenDoesNotBurn
                , TestLabel "BurnOnlyTenReturnsEmpty" testBurnOnlyTenReturnsEmpty
                , TestLabel "BurnTenOnCardsReturnsEmpty" testBurnTenOnCardsReturnsEmpty
                , TestLabel "BurnOnlyFourSixesReturnsEmpty" testBurnOnlyFourSixesReturnsEmpty
                , TestLabel "BurnFourJakcsOnOtherCardsReturnsEmpty" testBurnFourJakcsOnOtherCardsReturnsEmpty
                , TestLabel "BurnTenOnMoreThanFourCardsReturnsEmpty" testBurnTenOnMoreThanFourCardsReturnsEmpty
                ]

main = do
    runTestTT tests