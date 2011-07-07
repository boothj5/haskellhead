import Test.HUnit
import Data.Maybe
import PlayerCircle
import HumanPlayer
import Card
import Game
import Util

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
    HumanPlayer { name = "Mark", hand = [], faceUp = [], faceDown = []}
playerWithNoCards2 = 
    HumanPlayer { name = "Stevie", hand = [], faceUp = [], faceDown = []}
playerWithFaceDownCardsOnly = 
    HumanPlayer { name = "James", hand = [], faceUp = [], faceDown = [cardA]}
payerWithFaceUpAndFaceDownCards = 
    HumanPlayer { name = "Davo", hand = [], faceUp = [cardB], faceDown = [cardA]}
payerWithCardsInAllHands = 
    HumanPlayer { name = "Monkey", hand = [cardC], faceUp = [cardB], faceDown = [cardA]}

twoPlayersWithCards = [playerWithFaceDownCardsOnly, payerWithCardsInAllHands, playerWithNoCards]
onlyOnePlayerWithCards = [playerWithFaceDownCardsOnly, playerWithNoCards, playerWithNoCards]

gameInPlay = Game { numPlayers      = 3
                       ,players         = twoPlayersWithCards
                       ,numCardsEach    = 0
                       ,deck            = [] 
                       ,pile            = []
                       ,burnt            = []
                       ,lastMove        = "" }

gameNotInPlay = Game { numPlayers      = 3
                       ,players         = onlyOnePlayerWithCards
                       ,numCardsEach    = 0
                       ,deck            = [] 
                       ,pile            = []
                       ,burnt            = []
                       ,lastMove        = "" }


playerWithCards1 = 
    HumanPlayer { name = "Monkey", hand = [cardC], faceUp = [cardB], faceDown = [cardA]}
playerWithCards2 = 
    HumanPlayer { name = "Dude", hand = [cardC], faceUp = [cardB], faceDown = [cardA]}
playerWithCards3 = 
    HumanPlayer { name = "Bob", hand = [cardC], faceUp = [cardB], faceDown = [cardA]}

playerList1 = [playerWithCards1, playerWithCards2, playerWithCards3]
testNextPlayerWithCardsNextHasCards =
    TestCase (assertEqual
        "Test next player with cards is chosen when next player has cards"
        "Dude" (name $ head $ moveToNextPlayerWithCards playerList1 ))

playerList2 = [playerWithCards1, playerWithNoCards, playerWithCards2, playerWithCards3]
testNextPlayerWithCardsNextButOneHasCards =
    TestCase (assertEqual
        "Test next player with cards is chosen when next player but one has cards"
        "Dude" (name $ head $ moveToNextPlayerWithCards playerList2 ))

playerList3 = [playerWithCards1, playerWithNoCards, playerWithNoCards, playerWithCards3]
testNextPlayerWithCardsNextButTwoHasCards =
    TestCase (assertEqual
        "Test next player with cards is chosen when next player but two has cards"
        "Bob" (name $ head $ moveToNextPlayerWithCards playerList3 ))

playerList4 = [playerWithNoCards, playerWithNoCards, playerWithNoCards, playerWithCards3]
testNextPlayerWithCardsOnlyLastHasCards =
    TestCase (assertEqual
        "Test next player with cards is chosen when only last player has cards"
        "Bob" (name $ head $ moveToNextPlayerWithCards playerList4 ))

-- Suite
tests = TestList [TestLabel "NextTurn" testNextTurn
                , TestLabel "CompleteCicle " testCompleteCicle 
                , TestLabel "NextPlayerWithCardsNextHasCards" testNextPlayerWithCardsNextHasCards
                , TestLabel "NextPlayerWithCardsNextButOneHasCards" testNextPlayerWithCardsNextButOneHasCards
                , TestLabel "NextPlayerWithCardsNextButTwoHasCards" testNextPlayerWithCardsNextButTwoHasCards
                , TestLabel "NextPlayerWithCardsOnlyLastHasCards" testNextPlayerWithCardsOnlyLastHasCards
                ]

main = runTestTT tests