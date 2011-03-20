import Data.IORef
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Char

import Card
import Player
import Game
import State
import Console
import Util

main = do
    evalStateT startGame newGameST

startGame = do
    console clearScreen
    console welcome
    console line
    
    nplayers <- console getNumPlayers
    cards <- console getNumCards
    createDeckST cards nplayers
    
    getPlayerNames
    dealST
    
    console clearScreen
    showGame
    console showCardsDealt
    console waitUser
    
    swapAll
    
    makeFirstMove
    
    nextMove
    
    playerList <- gets players
    let shithead = getShithead playerList 
    case shithead of
       Nothing -> console showShitheadError
       _       -> console $ showShithead (name $ fromJust shithead)
       
-- Functions for game setup

showGame = do
    game <- get
    console $ showGameDetails game

getPlayerNames = do   
    n <- gets numPlayers
    playerNames <- forM [1..n] (\num -> do  
        console $ getPlayerName num)  
    createPlayersST playerNames 

-- Functions for swapping cards

doSwap player = do
    let theName = name player
    handCardToSwap <- console $ getSwapHand theName
    faceUpCardToSwap <- console $ getSwapFaceUp theName
    swapCardsST player (handCardToSwap-1) (faceUpCardToSwap-1)

    console clearScreen
    playerList <- gets players
    let newPlayer = fromJust $ getPlayer (name player) playerList
    console $ showPlayer newPlayer
    swapMore <- console $ askSwapMore theName
    (when (charToBoolean swapMore) $ 
        doSwap newPlayer)

swapAll = do
    playerList <- gets players
    forM playerList (\player -> do
        console clearScreen
        console (print player)
        let theName = name player
        swap <- console $ askSwap theName
        (when (charToBoolean swap) $ 
            doSwap player))
   
-- Function to perform first move   
   
makeFirstMove = do
    playerList <- gets players
    let player = playerWithLowestCardFromCircle playerList
        cards = lowestCards player    
    layCardsST player cards
    dealToHandST player (length cards)
    console $ showMove (name player) cards

-- Main game loop functions

nextMove = do
    moveToNextPlayerST
    ps <- gets players
    let currentPlayer = head ps 
    thePile <- gets pile
    console clearScreen
    showGame
    if playingFromFaceDown currentPlayer
        then moveFromFaceDown currentPlayer
        else if canMove currentPlayer thePile
                then makeMove currentPlayer
                else cantMove currentPlayer
    game <- get
    (when (inPlay game) nextMove)


makeMove player = do
    str <- console $ askMove (name player) 
    let cardsToPlay = getCards player (indexesFromString str)

    currentPile <- gets pile
    if not (validMove (head cardsToPlay) currentPile) || not (allRanksSame cardsToPlay)
        then do 
            console $ showBadMove cardsToPlay
            makeMove player
        else do
            layCardsST player cardsToPlay
            dealToHandST player (length cardsToPlay)

cantMove player = do
    console $ pickUpWait (name player)
    pickUpPileST player

moveFromFaceDown player = do
    thePile <- gets pile
    cardToPlay <- console $ askFaceDown (name player)
    let card = getCard player (cardToPlay-1)
    if validMove card thePile
       then do
           console $ waitChoiceOk card
           layCardsST player [card]
       else do
           console $ waitChoiceFail card
           pickUpPileST player
           pickUpFromFaceDownST player card     
