import Data.IORef
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
    game <- getGameDetailsST
    putStrLn $ show game

getGameInfo = do
    putStrLn "Enter number of players:"
    players <- fmap read getLine
    putStrLn "Enter number of cards per hand:"
    cards <- fmap read getLine
    createDeckST cards players
                       
getPlayerNames = do   
    n <- getGamePropertyST numPlayers
    playerNames <- forM [1..n] (\a -> do  
        putStrLn $ "Enter name for player " ++ show a ++ ":"  
        playerName <- getLine  
        return playerName)  
    createPlayersST playerNames 

doSwap p = do
    let theName = name p
    putStrLn $ theName ++ ", select a hand card to swap:"
    handCardToSwap <- fmap read getLine
    putStrLn $ theName ++ ", select a face up card to swap:"
    faceUpCardToSwap <- fmap read getLine
    swapCardsST p (handCardToSwap-1) (faceUpCardToSwap-1)
    putStrLn $ theName ++ ", do you want to swap more cards?"
    swapMore <- getLine
    if (charToBoolean swapMore) 
        then
            doSwap p
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
                doSwap p
            else 
                return ())
   
firstMove = do
    playerList <- getGamePropertyST players
    let p = playerWithLowestCardFromList playerList
        cs = getLowestCards p    
    layCardsST p cs
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
