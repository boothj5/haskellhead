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
                       ,lastMove        = "" }

gameNotInPlay = GameDetails { numPlayers      = 3
                       ,players         = onlyOnePlayerWithCards
                       ,numCardsEach    = 0
                       ,deck            = [] 
                       ,pile            = []
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

-- Suite
tests = TestList [TestLabel "NextTurn" testNextTurn
                , TestLabel "CompleteCicle " testCompleteCicle 
                , TestLabel "InPlayWhenTwoPlayersHaveCards" testInPlayWhenTwoPlayersHaveCards
                , TestLabel "NotInPlayWhenOnePlayerHasCards" testNotInPlayWhenOnePlayerHasCards
                , TestLabel "OneDeckRequired" testOneDeckRequired
                , TestLabel "TwoDeckRequired" testTwoDeckRequired
                ]

main = do
    runTestTT tests