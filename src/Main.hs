import Data.IORef

import System.Random
import System.Random.Shuffle
import Control.Monad
import Game
import State

------------------------------------------------
--
-- IO Actions, either manipulate the game stateST,
-- or interact with the user
--

clearScreen = do
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

showGame = do
    game <- readIORef stateST
    putStrLn $ show game

getGameInfo = do
    putStrLn "Enter number of players:"
    players <- fmap read getLine
    putStrLn "Enter number of cards per hand:"
    cards <- fmap read getLine

    gen <- getStdGen
    let newDeck = newDeckWithEnoughCards $ numDecksRequired cards players
        shuffledDeck = shuffle' newDeck (length newDeck) gen

    modifyGameST $ \st ->
                    st { numPlayers     = players
                        ,numCardsEach   = cards
                        ,deck           = shuffledDeck }
                       
getPlayerNames = do   
    n <- getGamePropertyST numPlayers
    
    playerNames <- forM [1..n] (\a -> do  
        putStrLn $ "Enter name for player " ++ show a ++ ":"  
        playerName <- getLine  
        return playerName)  
    
    let newPlayers = createPlayers playerNames 
    modifyGameST $ \st -> st { players = newPlayers } 

doSwap playerList p = do
    let theName = name p
    putStrLn $ theName ++ ", select a hand card to swap:"
    handCardToSwap <- fmap read getLine
    putStrLn $ theName ++ ", select a face up card to swap:"
    faceUpCardToSwap <- fmap read getLine

    let swappedPlayers = swapForNamedPlayer p playerList (handCardToSwap-1) (faceUpCardToSwap-1)
    modifyGameST $ \st -> st { players = swappedPlayers }

    putStrLn $ theName ++ ", do you want to swap more cards?"
    swapMore <- getLine

    if (charToBoolean swapMore) 
        then
            doSwap playerList p
        else 
            return ()


swapAll = do
    playerList <- getGamePropertyST players

    forM playerList (\p -> do
        theName <- return $ name p

        putStrLn $ theName ++ ", do you want to swap cards?"
        swap <- getLine

        if (charToBoolean swap) 
            then 
                doSwap playerList p
            else 
                return ())
   
firstMove = do
    playerList <- getGamePropertyST players
    oPile <- getGamePropertyST pile

    let p = playerWithLowestCardFromList playerList
        cs = getLowestCards p    
        nPile = cs ++ oPile
        nPlayerList = removeFromNamedPlayersHand p playerList cs
        nPlayerList2 = makeCurrentPlayer p nPlayerList

    modifyGameST $ \st -> 
                    st { pile    = nPile 
                        ,players = nPlayerList2 }
    dealToHandST p

    putStrLn $ show (name p) ++ " laid the " ++ show cs


main = do
    clearScreen
    putStrLn "Welcome to Haskellhead!"
    putStrLn ""

    getGameInfo
    putStrLn ""

    getPlayerNames

    dealST

    clearScreen
    showGame
    putStrLn ""
    putStrLn "Press enter to continue"
    getLine

    swapAll

    firstMove

    showGame
    


--    withGameST $ \st -> do
--        case numCardsEach st > 3 of
--            True -> putStrLn "More than 3 cards"   
--            False -> putStrLn "Less than or 3 cards"

