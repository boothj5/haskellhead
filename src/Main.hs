import Data.IORef
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Char

import Card
import Player
import Game
import State
import Util

main = do
    evalStateT startGame newGameST

startGame = do
    clearScreen
    lift $ putStrLn "Welcome to Haskellhead!"
    lift $ putStrLn ""
    lift $ putStrLn "How many players?"
    nplayers <- lift $ fmap read getLine
    lift $ putStrLn "How many cards each?"
    cards <- lift $ fmap read getLine

    createDeckST cards nplayers
    
    getPlayerNames

    dealST

    clearScreen
    showGame
    lift $ putStrLn "Cards dealt, press enter:"
    lift $ getLine

    swapAll
    makeFirstMove
 
    nextMove

    playerList <- gets players
    let shithead = getShithead playerList 
    if shithead == Nothing
       then lift $ putStrLn "ERROR - NO SHITHEAD :/"
       else lift $ putStrLn $ show (name $ fromJust shithead) ++ " IS A SHITHEAD!!!!!!!!!!!!"
       
-- Functions for game setup

clearScreen = do
    lift $ putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    lift $ putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    lift $ putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

showGame = do
    game <- get
    lift $ (print game)

getPlayerNames = do   
    n <- gets numPlayers
    playerNames <- forM [1..n] (\a -> do  
        lift $ putStrLn $ "Enter name for player " ++ show a ++ ":"  
        lift $ getLine)  
    createPlayersST playerNames 

-- Functions for swapping cards

doSwap player = do
    let theName = name player
    lift $ putStrLn $ theName ++ ", select a hand card to swap:"
    handCardToSwap <- lift $ fmap read getLine
    lift $ putStrLn $ theName ++ ", select a face up card to swap:"
    faceUpCardToSwap <- lift $ fmap read getLine
    swapCardsST player (handCardToSwap-1) (faceUpCardToSwap-1)
    clearScreen
    playerList <- gets players
    let newPlayer = fromJust $ getPlayer (name player) playerList
    lift $ (print newPlayer)
    lift $ putStrLn $ theName ++ ", do you want to swap more cards?"
    swapMore <- lift $ getLine
    (when (charToBoolean swapMore) $ 
        doSwap newPlayer)

swapAll = do
    playerList <- gets players
    forM playerList (\player -> do
        clearScreen
        lift $ (print player)
        let theName = name player
        lift $ putStrLn $ theName ++ ", do you want to swap cards?"
        swap <- lift $ getLine
        (when (charToBoolean swap) $ 
            doSwap player))
   
-- Function to perform first move   
   
makeFirstMove = do
    playerList <- gets players
    let player = playerWithLowestCardFromCircle playerList
        cards = lowestCards player    
    layCardsST player cards
    dealToHandST player (length cards)
    lift $ putStrLn $ show (name player) ++ " laid the " ++ show cards

-- Main game loop functions

nextMove = do
    moveToNextPlayerST
    ps <- gets players
    let currentPlayer = head ps 
    thePile <- gets pile
    clearScreen
    showGame
    if playingFromFaceDown currentPlayer
        then moveFromFaceDown currentPlayer
        else if canMove currentPlayer thePile
                then makeMove currentPlayer
                else cantMove currentPlayer
    game <- get
    (when (inPlay game) nextMove)


makeMove player = do
    lift $ putStrLn $ name player ++ ", which cards do you wish to lay?"
    str <- lift $ getLine
    let cardsToPlay = getCards player (indexesFromString str)
    currentPile <- gets pile
    if not (validMove (head cardsToPlay) currentPile) || not (allRanksSame cardsToPlay)
        then do 
            lift $ putStrLn $ "You cannot lay " ++ show cardsToPlay
            makeMove player
        else do
            layCardsST player cardsToPlay
            dealToHandST player (length cardsToPlay)

cantMove player = do
    lift $ putStrLn $ "OH DEAR! " ++ name player ++ ", you cannot move."
    lift $ putStrLn "Press enter to pick up the pile."
    lift $ getLine
    pickUpPileST player
    

moveFromFaceDown player = do
    thePile <- gets pile
    lift $ putStrLn $ name player ++ ", which card do you wish choose?"
    cardToPlay <- lift $ fmap read getLine
    let card = getCard player (cardToPlay-1)
    if validMove card thePile
       then do
           lift $ putStrLn $ "Whew you chose the " ++ show card ++ ", press enter,"
           lift $ getLine
           layCardsST player [card]
       else do
           lift $ putStrLn $ "OH DEAR! You chose the " ++ show card ++ ", press enter,"
           lift $ getLine
           pickUpPileST player
           pickUpFromFaceDownST player card     
