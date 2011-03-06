import Test.HUnit
import Game

-- test player equality
player1 = ( Player { name = "James", hand = [], faceUp = [], faceDown = []} )
player2 = ( Player { name = "James", hand = [(Card Three Spades)], faceUp = [], faceDown = []} )
player3 = ( Player { name = "Monkey", hand = [(Card Three Spades)], faceUp = [], faceDown = []} )
player4 = ( Player { name = "james", hand = [], faceUp = [], faceDown = []} )
testPlayersEqualDiffCards = 
    TestCase (assertEqual
        "Players equal with different cards"
        player1 player2)

testPlayersNotEqualDiffCase = 
    TestCase (assertBool
        "Players not equal with different case"
        (player1 /= player4))

testPlayersNotEqualDiffNames = 
    TestCase (assertBool
        "Players not equal with different names"
        (player1 /= player3))

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
playerWithOnlyHand =
        ( Player { name = "Sloth", hand = [cardC], faceUp = [], faceDown = []} )
playerWithHandAndFaceUp =
        ( Player { name = "Sloth", hand = [cardC], faceUp = [cardB], faceDown = []} )
    

testPlayerHasCardsWhenFaceDownOnly = 
    TestCase (assertBool
        "Test player has cards when faceDown only"
        (hasCards playerWithFaceDownCardsOnly))

testPlayerHasCardsWhenFaceDownAndFaceUp = 
    TestCase (assertBool
        "Test player has cards when faceDown and faceUp"
        (hasCards payerWithFaceUpAndFaceDownCards))

testPlayerHasCardsWhenAllHands = 
    TestCase (assertBool
        "Test player has cards when all hands"
        (hasCards payerWithCardsInAllHands))

testNotPlayingFromFaceDownWhenAllHandsHaveCards = 
    TestCase (assertBool
        "Test not playing from face down when all hands have cards"
        ((playingFromFaceDown payerWithCardsInAllHands) == False))

testNotPlayingFromFaceDownWhenOnlyEmptyHand = 
    TestCase (assertBool
        "Test not playing from face down when only empty hand"
        ((playingFromFaceDown payerWithFaceUpAndFaceDownCards) == False))

testNotPlayingFromFaceDownOnlyCardsInHand = 
    TestCase (assertBool
        "Test not playing from face down when only cards in hand"
        ((playingFromFaceDown playerWithOnlyHand) == False))

testNotPlayingFromFaceDownCardsInHandAndFaceUp = 
    TestCase (assertBool
        "Test not playing from face down when cards in hand and faceup"
        ((playingFromFaceDown playerWithHandAndFaceUp) == False))

testPlayingFromFaceDownWhenOnlyFaceDown = 
    TestCase (assertBool
        "Test playing from face down when only cards in facedown"
        (playingFromFaceDown playerWithFaceDownCardsOnly))

   
-- Suite
tests = TestList [TestLabel "PlayersEqualDiffCards" testPlayersEqualDiffCards
                , TestLabel "PlayersNotEqualDiffCase" testPlayersNotEqualDiffCase
                , TestLabel "PlayersNotEqualDiffNames" testPlayersNotEqualDiffNames
                , TestLabel "PlayerHasCardsWhenFaceDownOnly" testPlayerHasCardsWhenFaceDownOnly
                , TestLabel "PlayerHasCardsWhenFaceDownAndFaceUp" testPlayerHasCardsWhenFaceDownAndFaceUp
                , TestLabel "PlayerHasCardsWhenAllHands" testPlayerHasCardsWhenAllHands
                , TestLabel "NotPlayingFromFaceDownWhenAllHandsHaveCards" testNotPlayingFromFaceDownWhenAllHandsHaveCards
                , TestLabel "NotPlayingFromFaceDownWhenOnlyEmptyHand" testNotPlayingFromFaceDownWhenOnlyEmptyHand
                , TestLabel "NotPlayingFromFaceDownOnlyCardsInHand" testNotPlayingFromFaceDownOnlyCardsInHand
                , TestLabel "NotPlayingFromFaceDownCardsInHandAndFaceUp" testNotPlayingFromFaceDownCardsInHandAndFaceUp
                , TestLabel "PlayingFromFaceDownWhenOnlyFaceDown" testPlayingFromFaceDownWhenOnlyFaceDown
                ]

main = do
    runTestTT tests