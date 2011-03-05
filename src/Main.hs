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

getPlayerNames = do   
    n <- getGamePropertyST numPlayers
    playerNames <- forM [1..n] (\a -> do  
        putStrLn $ "Enter name for player " ++ show a ++ ":"  
        playerName <- getLine  
        return playerName)  
    createPlayersST playerNames 

doSwap player = do
    let theName = name player
    putStrLn $ theName ++ ", select a hand card to swap:"
    handCardToSwap <- fmap read getLine
    putStrLn $ theName ++ ", select a face up card to swap:"
    faceUpCardToSwap <- fmap read getLine
    swapCardsST player (handCardToSwap-1) (faceUpCardToSwap-1)
    putStrLn $ theName ++ ", do you want to swap more cards?"
    swapMore <- getLine
    if (charToBoolean swapMore) 
        then
            doSwap player
        else 
            return ()

swapAll = do
    playerList <- getGamePropertyST players
    forM playerList (\player -> do
        let theName = name player
        putStrLn $ theName ++ ", do you want to swap cards?"
        swap <- getLine
        if (charToBoolean swap) 
            then 
                doSwap player
            else 
                return ())
   
makeFirstMove = do
    playerList <- getGamePropertyST players
    let player = playerWithLowestCardFromList playerList
        cards = getLowestCards player    
    layCardsST player cards
    dealToHandST player (length cards)
    putStrLn $ show (name player) ++ " laid the " ++ show cards

main = do
    clearScreen
    putStrLn "Welcome to Haskellhead!"
    putStrLn ""
    putStrLn "Enter number of players:"
    players <- fmap read getLine
    putStrLn "Enter number of cards per hand:"
    cards <- fmap read getLine

    createDeckST cards players
    
    putStrLn ""

    n <- getGamePropertyST numPlayers
    playerNames <- forM [1..n] (\a -> do  
        putStrLn $ "Enter name for player " ++ show a ++ ":"  
        playerName <- getLine  
        return playerName)  
    createPlayersST playerNames 

    dealST

    clearScreen
    showGame
    putStrLn ""
    putStrLn "Press enter to continue"
    getLine

    swapAll
    makeFirstMove
    nextPlayer <- moveToNextPlayerST
    
    clearScreen
    showGame
    putStrLn $ (name nextPlayer) ++ ", which card do you wish to lay?"
