import Data.IORef
import Control.Monad
import Data.Maybe
import Data.Char

import Card
import Player
import Game
import State
import Util

main = do
    clearScreen
    putStrLn "Welcome to Haskellhead!"
    putStrLn ""
    putStrLn "How many players?"
    nplayers <- fmap read getLine
    putStrLn "How many cards each?"
    cards <- fmap read getLine

    createDeckST cards nplayers
    
    n <- getGamePropertyST numPlayers
    playerNames <- forM [1..n] (\a -> do  
        putStrLn $ "Enter name for player " ++ show a ++ ":"  
        getLine)  
    createPlayersST playerNames 

    dealST

    clearScreen
    showGame
    putStrLn "Cards dealt, press enter:"
    getLine

    swapAll
    makeFirstMove
 
    nextMove

    playerList <- getGamePropertyST players
    let shithead = getShithead playerList 
    if shithead == Nothing
       then putStrLn "ERROR - NO SHITHEAD :/"
       else putStrLn $ show (name $ fromJust shithead) ++ " IS A SHITHEAD!!!!!!!!!!!!"
       
-- Functions for game setup

clearScreen = do
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

showGame = do
    game <- getGameST
    (print game)

getPlayerNames = do   
    n <- getGamePropertyST numPlayers
    playerNames <- forM [1..n] (\a -> do  
        putStrLn $ "Enter name for player " ++ show a ++ ":"  
        getLine)  
    createPlayersST playerNames 

-- Functions for swapping cards

doSwap player = do
    let theName = name player
    putStrLn $ theName ++ ", select a hand card to swap:"
    handCardToSwap <- fmap read getLine
    putStrLn $ theName ++ ", select a face up card to swap:"
    faceUpCardToSwap <- fmap read getLine
    swapCardsST player (handCardToSwap-1) (faceUpCardToSwap-1)
    clearScreen
    playerList <- getGamePropertyST players
    let newPlayer = fromJust $ getPlayer (name player) playerList
    (print newPlayer)
    putStrLn $ theName ++ ", do you want to swap more cards?"
    swapMore <- getLine
    (when (charToBoolean swapMore) $ 
        doSwap newPlayer)

swapAll = do
    playerList <- getGamePropertyST players
    forM playerList (\player -> do
        clearScreen
        (print player)
        let theName = name player
        putStrLn $ theName ++ ", do you want to swap cards?"
        swap <- getLine
        (when (charToBoolean swap) $ 
            doSwap player))
   
-- Function to perform first move   
   
makeFirstMove = do
    playerList <- getGamePropertyST players
    let player = playerWithLowestCardFromCircle playerList
        cards = lowestCards player    
    layCardsST player cards
    dealToHandST player (length cards)
    putStrLn $ show (name player) ++ " laid the " ++ show cards

-- Main game loop functions

nextMove = do
    currentPlayer <- moveToNextPlayerST
    thePile <- getGamePropertyST pile
    clearScreen
    showGame
    if playingFromFaceDown currentPlayer
        then moveFromFaceDown currentPlayer
        else if canMove currentPlayer thePile
                then makeMove currentPlayer
                else cantMove currentPlayer
    game <- getGameST
    (when (inPlay game) nextMove)


makeMove player = do
    putStrLn $ name player ++ ", which cards do you wish to lay?"
    str <- getLine
    let cardsToPlay = getCards player (indexesFromString str)
    currentPile <- getGamePropertyST pile
    if not (validMove (head cardsToPlay) currentPile) || not (allRanksSame cardsToPlay)
        then do 
            putStrLn $ "You cannot lay " ++ show cardsToPlay
            makeMove player
        else do
            layCardsST player cardsToPlay
            dealToHandST player (length cardsToPlay)

cantMove player = do
    putStrLn $ "OH DEAR! " ++ name player ++ ", you cannot move."
    putStrLn "Press enter to pick up the pile."
    getLine
    pickUpPileST player
    

moveFromFaceDown player = do
    thePile <- getGamePropertyST pile
    putStrLn $ name player ++ ", which card do you wish choose?"
    cardToPlay <- fmap read getLine
    let card = getCard player (cardToPlay-1)
    if validMove card thePile
       then do
           putStrLn $ "Whew you chose the " ++ show card ++ ", press enter,"
           getLine
           layCardsST player [card]
       else do
           putStrLn $ "OH DEAR! You chose the " ++ show card ++ ", press enter,"
           getLine
           pickUpPileST player
           pickUpFromFaceDownST player card     
