import Test.HUnit
import Game

simpleTest = TestCase (assertEqual "Simple test, " [2] (2:[]))

initial = [1,2,3,4]
expected = [2,3,4,1]
testNextTurn = TestCase (assertEqual "Test next turn, " expected (nextTurn initial))


tests = TestList [TestLabel "simpleTest" simpleTest, TestLabel "testNextTurn" testNextTurn]

main = do
    runTestTT tests
