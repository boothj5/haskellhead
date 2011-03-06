import Data.IORef
import Control.Monad
import Data.Maybe
import Game
import State

main = do
    clearScreen
    putStrLn "Welcome to Haskellhead!"
    putStrLn ""
    putStrLn "Enter number of players:"
    nplayers <- fmap read getLine
    putStrLn "Enter number of cards per hand:"
    cards <- fmap read getLine

    createDeckST cards nplayers
    
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

    playerList <- getGamePropertyST players
    let shithead = getShithead playerList 
    if (shithead == Nothing)
       then putStrLn "ERROR - NO SHITHEAD :/"
       else putStrLn $ (show $ name $ fromJust shithead) ++ " IS A SHITHEAD!!!!!!!!!!!!"
       
-- Functions for game setup

clearScreen = do
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

showGame = do
    game <- getGameST
    putStrLn $ show game

getPlayerNames = do   
    n <- getGamePropertyST numPlayers
    playerNames <- forM [1..n] (\a -> do  
        putStrLn $ "Enter name for player " ++ show a ++ ":"  
        playerName <- getLine  
        return playerName)  
    createPlayersST playerNames 

-- Functions for swapping cards

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
   
-- Function to perform first move   
   
makeFirstMove = do
    playerList <- getGamePropertyST players
    let player = playerWithLowestCardFromList playerList
        cards = getLowestCards player    
    layCardsST player cards
    dealToHandST player (length cards)
    putStrLn $ show (name player) ++ " laid the " ++ show cards

-- Main game loop functions

nextMove = do
    currentPlayer <- moveToNextPlayerST
    thePile <- getGamePropertyST pile
    clearScreen
    showGame
    if (playingFromFaceDown currentPlayer)
        then moveFromFaceDown currentPlayer
        else if (canMove currentPlayer thePile)
                then makeMove currentPlayer
                else cantMove currentPlayer
    game <- getGameST
    if (inPlay game)
        then nextMove
        else return ()

moveFromFaceDown player = do
    thePile <- getGamePropertyST pile
    putStrLn $ (name player) ++ ", which card do you wish choose?"
    cardToPlay <- fmap read getLine
    let card = getCard player (cardToPlay-1)
    if (validMove card thePile)
       then do
           putStrLn $ "Whew you chose the " ++ show card ++ ", press enter,"
           layCardsST player (card:[])
       else do
           putStrLn $ "OH DEAR! You chose the " ++ show card ++ ", press enter,"
           pickUpPileST player
           pickUpFromFaceDownST player card 

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

cantMove player = do
    putStrLn $ "OH DEAR! " ++ (name player) ++ ", you cannot move."
    putStrLn "Press enter to pick up the pile."
    getLine
    pickUpPileST player
