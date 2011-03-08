import Test.HUnit
import Card
import Game

-- some short cuts for cards
two = Card Two Spades
three = Card Three Hearts
four = Card Four Spades
five = Card Five Hearts
six = Card Six Clubs
seven = Card Seven Diamonds
eight = Card Eight Spades
nine = Card Nine Hearts
ten = Card Ten Clubs
jack = Card Jack Clubs
queen = Card Queen Hearts
king = Card King Diamonds
ace = Card Ace Diamonds

-- test special cards
testBurnCardIsSpecial = 
    TestCase (assertBool
        "Test burn card is special card"
        (layOnAnythingCard (Card burnRank Hearts)))

testInvisibleCardIsSpecial = 
    TestCase (assertBool
        "Test invisble card is special card"
        (layOnAnythingCard (Card invisibleRank Hearts)))

testResetCardIsSpecial = 
    TestCase (assertBool
        "Test reset card is special card"
        (layOnAnythingCard (Card resetRank Hearts)))
                            
testThreeNotSpecial = 
    TestCase (assertBool
        "Test three is not special"
        (not $ layOnAnythingCard (Card Three Diamonds)))

testAceNotSpecial = 
    TestCase (assertBool
        "Test ace is not special"
        (not $ layOnAnythingCard (Card Ace Spades)))


-- Test valid moves
testThreeOnNothingValid = 
    TestCase (assertBool
        "Test laying three on empty pile"
        (validMove three []))

testEightOnNothingValid = 
    TestCase (assertBool
        "Test laying eight on empty pile"
        (validMove eight []))

testSevenOnNothingValid = 
    TestCase (assertBool
        "Test laying eight on empty pile"
        (validMove seven []))
   
testTenOnNothingValid = 
    TestCase (assertBool
        "Test laying ten on empty pile"
        (validMove ten []))
   
testTwoOnThreeValid = 
    TestCase (assertBool
        "Test laying two on three is valid"
        (validMove two [three]))

testTwoOnAceValid = 
    TestCase (assertBool
        "Test laying two on ace is valid"
        (validMove two [ace]))

testTwoOnSevenOnAceValid = 
    TestCase (assertBool
        "Test laying two on a seven on an ace is valid"
        (validMove two [seven, ace]))

testFourOnSevenOnNothingValid = 
    TestCase (assertBool
        "Test laying four on a seven on nothing is valid"
        (validMove four [seven]))

testNineOnThreeSevensAndSixValid = 
    TestCase (assertBool
        "Test laying nine on three sevens and six is valid"
        (validMove nine [seven, seven, seven, six]))

testNineOnThreeSevensAndJackNotValid = 
    TestCase (assertBool
        "Test laying nine on three sevens and jack is valid"
        (not $ validMove nine [seven, seven, seven, jack]))

testSevenOnThreeValid = 
    TestCase (assertBool
        "Test laying seven on three is valid"
        (validMove seven [three]))

testTenOnKingValid = 
    TestCase (assertBool
        "Test laying ten on king is valid"
        (validMove ten [king]))

testFourOnFourValid = 
    TestCase (assertBool
        "Test laying four on four is valid"
        (validMove four [four, ace]))

testJackOnQueenNotValid = 
    TestCase (assertBool
        "Test laying jack on queen is not valid"
        (not $ validMove jack [queen, ace]))

testSameRankFalseWhenEmptyList =
    TestCase (assertEqual
        "Test same rank returns false when empty list"
        False (allRanksSame []))

testSameRankTrueWhenOneCard =
    TestCase (assertEqual
        "Test same rank returns true when one card"
        True (allRanksSame [Card Three Diamonds]))

testSameRankTrueWhenTwoSame =
    TestCase (assertEqual
        "Test same rank returns true when two same"
        True (allRanksSame [Card Three Diamonds, Card Three Spades]))

testSameRankTrueWhenThreeSame =
    TestCase (assertEqual
        "Test same rank returns true when three same"
        True (allRanksSame [Card Three Diamonds, Card Three Spades, Card Three Hearts]))

testSameRankFalseWhenTwoDifferent =
    TestCase (assertEqual
        "Test same rank returns false when two different"
        False (allRanksSame [Card Three Diamonds, Card Four Spades]))

testSameRankFalseWhenThreeDifferent =
    TestCase (assertEqual
        "Test same rank returns false when three different"
        False (allRanksSame [Card Three Diamonds, Card Four Spades, Card Jack Diamonds]))

testSameRankFalseWhenThreeSameOneDifferent =
    TestCase (assertEqual
        "Test same rank returns false when three same, one different"
        False (allRanksSame [Card Three Diamonds, Card Three Spades, Card Three Clubs, Card Four Diamonds]))

testSameRankFalseWhenThreeSameOneDifferentOneMoreSame =
    TestCase (assertEqual
        "Test same rank returns false when three same, one different, and one more same"
        False (allRanksSame [Card Three Diamonds
                       , Card Three Spades
                       , Card Three Clubs
                       , Card Four Diamonds
                       , Card Three Hearts]))


-- Suite
tests = TestList [TestLabel "BurnCardIsSpecial" testBurnCardIsSpecial
                , TestLabel "InvisibleCardIsSpecial" testInvisibleCardIsSpecial
                , TestLabel "ResetCardIsSpecial" testResetCardIsSpecial
                , TestLabel "ThreeNotSpecial" testThreeNotSpecial
                , TestLabel "AceNotSpecial" testAceNotSpecial
                , TestLabel "ThreeOnNothingValid" testThreeOnNothingValid
                , TestLabel "EightOnNothingValid" testEightOnNothingValid
                , TestLabel "SevenOnNothingValid" testSevenOnNothingValid
                , TestLabel "TenOnNothingValid" testTenOnNothingValid
                , TestLabel "TwoOnThreeValid" testTwoOnThreeValid
                , TestLabel "TwoOnAceValid" testTwoOnAceValid
                , TestLabel "TwoOnSevenOnAceValid" testTwoOnSevenOnAceValid
                , TestLabel "FourOnSevenOnNothingValid" testFourOnSevenOnNothingValid
                , TestLabel "NineOnThreeSevensAndSixValid" testNineOnThreeSevensAndSixValid
                , TestLabel "NineOnThreeSevensAndJackNotValid" testNineOnThreeSevensAndJackNotValid
                , TestLabel "SevenOnThreeValid" testSevenOnThreeValid
                , TestLabel "TenOnKingValid" testTenOnKingValid
                , TestLabel "FourOnFourValid" testFourOnFourValid
                , TestLabel "JackOnQueenNotValid" testJackOnQueenNotValid
                , TestLabel "SameRankFalseWhenEmptyList" testSameRankFalseWhenEmptyList
                , TestLabel "SameRankTrueWhenOneCard" testSameRankTrueWhenOneCard
                , TestLabel "SameRankTrueWhenTwoSame" testSameRankTrueWhenTwoSame
                , TestLabel "SameRankTrueWhenThreeSame" testSameRankTrueWhenThreeSame
                , TestLabel "SameRankFalseWhenTwoDifferent" testSameRankFalseWhenTwoDifferent
                , TestLabel "SameRankFalseWhenThreeDifferent" testSameRankFalseWhenThreeDifferent
                , TestLabel "SameRankFalseWhenThreeSameOneDifferent" testSameRankFalseWhenThreeSameOneDifferent
                ]

main = do
    runTestTT tests
