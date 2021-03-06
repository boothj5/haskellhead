import Test.HUnit
import Game
import HumanPlayer
import Player
import Card

-- test player equality
player1 = HumanPlayer { name = "James", hand = [], faceUp = [], faceDown = []}
player2 = HumanPlayer { name = "James", hand = [Card Three Spades], faceUp = [], faceDown = []}
player3 = HumanPlayer { name = "Monkey", hand = [Card Three Spades], faceUp = [], faceDown = []}
player4 = HumanPlayer { name = "james", hand = [], faceUp = [], faceDown = []}
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
    HumanPlayer { name = "Mark", hand = [], faceUp = [], faceDown = []}
playerWithNoCards2 = 
    HumanPlayer { name = "Stevie", hand = [], faceUp = [], faceDown = []}
playerWithFaceDownCardsOnly = 
    HumanPlayer { name = "James", hand = [], faceUp = [], faceDown = [cardA]}
payerWithFaceUpAndFaceDownCards = 
    HumanPlayer { name = "Davo", hand = [], faceUp = [cardB], faceDown = [cardA]}
payerWithCardsInAllHands = 
    HumanPlayer { name = "Monkey", hand = [cardC], faceUp = [cardB], faceDown = [cardA]}
playerWithOnlyHand =
    HumanPlayer { name = "Sloth", hand = [cardC], faceUp = [], faceDown = []}
playerWithHandAndFaceUp =
    HumanPlayer { name = "Sloth", hand = [cardC], faceUp = [cardB], faceDown = []}
    

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
        (not (playingFromFaceDown payerWithCardsInAllHands)))

testNotPlayingFromFaceDownWhenOnlyEmptyHand = 
    TestCase (assertBool
        "Test not playing from face down when only empty hand"
        (not (playingFromFaceDown payerWithFaceUpAndFaceDownCards)))

testNotPlayingFromFaceDownOnlyCardsInHand = 
    TestCase (assertBool
        "Test not playing from face down when only cards in hand"
        (not (playingFromFaceDown playerWithOnlyHand)))

testNotPlayingFromFaceDownCardsInHandAndFaceUp = 
    TestCase (assertBool
        "Test not playing from face down when cards in hand and faceup"
        (not (playingFromFaceDown playerWithHandAndFaceUp)))

testPlayingFromFaceDownWhenOnlyFaceDown = 
    TestCase (assertBool
        "Test playing from face down when only cards in facedown"
        (playingFromFaceDown playerWithFaceDownCardsOnly))

playerToSwap = HumanPlayer { name = "Monkey"
                      , hand = [Card Ace Spades, Card Three Diamonds, Card Ten Spades]
                      , faceUp = [Card Six Spades, Card Two Hearts, Card Four Diamonds]
                      , faceDown = [Card Seven Clubs, Card Nine Diamonds, Card King Clubs]
                      } 

testSwapCardsHandCorrect =
    TestCase (assertBool
        "Test swapping cards result in correct hand"
        (Card Two Hearts `elem` returned 
            && Card Three Diamonds `elem` returned
            && Card Ten Spades `elem` returned))
    where returned = hand (swapHandWithFaceUp playerToSwap 0 1)
   
testSwapCardsFaceUpCorrect =
    TestCase (assertEqual
        "Test swapping cards result in correct faceUp"
        [Card Six Spades, Card Ace Spades, Card Four Diamonds]
        returned)
    where returned = faceUp (swapHandWithFaceUp playerToSwap 0 1)

beforeRemove = [ Card Ace Spades
               , Card Three Diamonds
               , Card Ten Spades
               , Card Seven Clubs
               , Card Ten Diamonds
               , Card King Clubs ]

toRemove = [ Card Three Diamonds
           , Card Ten Spades
           , Card Ten Diamonds ]

playerWithFaceDownForRemove = 
    HumanPlayer { name = "Monkey"
           , hand = []
           , faceUp = []
           , faceDown = beforeRemove
           } 

playerWithFaceUpForRemove = 
    HumanPlayer { name = "Monkey"
           , hand = []
           , faceUp = beforeRemove
           , faceDown = []
           } 

playerWithHandForRemove = 
    HumanPlayer { name = "Monkey"
           , hand = beforeRemove
           , faceUp = []
           , faceDown = []
           } 

testRemoveFromFaceDown = 
    TestCase (assertEqual
        "Test removing from face down"
        [ Card Ace Spades, Card Seven Clubs, Card King Clubs]
        (faceDown (removeFromFaceDown playerWithFaceDownForRemove toRemove)))

testRemoveFromFaceUp = 
    TestCase (assertEqual
        "Test removing from face up"
        [ Card Ace Spades, Card Seven Clubs, Card King Clubs]
        (faceUp (removeFromFaceUp playerWithFaceUpForRemove toRemove)))

testRemoveFromHand = 
    TestCase (assertEqual
        "Test removing from hand"
        [ Card Ace Spades, Card Seven Clubs, Card King Clubs]
        (hand (removeFromHand playerWithHandForRemove toRemove)))


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
                , TestLabel "SwapCardsHandCorrect" testSwapCardsHandCorrect
                , TestLabel "SwapCardsFaceUpCorrect" testSwapCardsFaceUpCorrect
                , TestLabel "RemoveFromFaceDown" testRemoveFromFaceDown
                , TestLabel "RemoveFromFaceUp" testRemoveFromFaceUp
                , TestLabel "RemoveFromHand" testRemoveFromHand
                ]

main = runTestTT tests