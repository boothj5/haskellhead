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

makeMove player = do
    putStrLn $ (name player) ++ ", which card do you wish to lay?"
    cardToPlay <- fmap read getLine
    let card = getCard player (cardToPlay-1)
    currentPile <- getGamePropertyST pile
    if (not $ validMove card currentPile)
        then do 
            putStrLn $ "You cannot lay the " ++ show card
            makeMove player
        else do
            layCardsST player (card:[])
            dealToHandST player 1
            return ()

cantMove player = do
    putStrLn $ "OH DEAR! " ++ (name player) ++ ", you cannot move."
    putStrLn "Press enter to pick up the pile."
    getLine
    pickUpPileST player
    nextMove

nextMove = do
    currentPlayer <- moveToNextPlayerST
    thePile <- getGamePropertyST pile
    clearScreen
    showGame
    if (canMove currentPlayer thePile)
        then makeMove currentPlayer
        else cantMove currentPlayer
    game <- getGameDetailsST
    if (inPlay game)
        then nextMove
        else return ()

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
 
    nextMove
