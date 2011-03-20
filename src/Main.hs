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
    lift $ clearScreen
    lift $ welcome
    lift $ line
    
    nplayers <- lift $ getNumPlayers
    cards <- lift $ getNumCards
    createDeckST cards nplayers
    
    getPlayerNames
    dealST
    
    lift $ clearScreen
    showGame
    lift $ showCardsDealt
    lift $ waitUser
    
    swapAll
    
    makeFirstMove
    
    nextMove
    
    playerList <- gets players
    let shithead = getShithead playerList 
    case shithead of
       Nothing -> lift $ showShitheadError
       _       -> lift $ showShithead (name $ fromJust shithead)
       
-- Functions for game setup

showGame = do
    game <- get
    lift $ showGameDetails game

getPlayerNames = do   
    n <- gets numPlayers
    playerNames <- forM [1..n] (\num -> do  
        lift $ getPlayerName num)  
    createPlayersST playerNames 

-- Functions for swapping cards

doSwap player = do
    let theName = name player
    handCardToSwap <- lift $ getSwapHand theName
    faceUpCardToSwap <- lift $ getSwapFaceUp theName
    swapCardsST player (handCardToSwap-1) (faceUpCardToSwap-1)

    lift $ clearScreen
    playerList <- gets players
    let newPlayer = fromJust $ getPlayer (name player) playerList
    lift $ showPlayer newPlayer
    swapMore <- lift $ askSwapMore theName
    (when (charToBoolean swapMore) $ 
        doSwap newPlayer)

swapAll = do
    playerList <- gets players
    forM playerList (\player -> do
        lift $ clearScreen
        lift $ (print player)
        let theName = name player
        swap <- lift $ askSwap theName
        (when (charToBoolean swap) $ 
            doSwap player))
   
-- Function to perform first move   
   
makeFirstMove = do
    playerList <- gets players
    let player = playerWithLowestCardFromCircle playerList
        cards = lowestCards player    
    layCardsST player cards
    dealToHandST player (length cards)
    lift $ showMove (name player) cards

-- Main game loop functions

nextMove = do
    moveToNextPlayerST
    ps <- gets players
    let currentPlayer = head ps 
    thePile <- gets pile
    lift $ clearScreen
    showGame
    if playingFromFaceDown currentPlayer
        then moveFromFaceDown currentPlayer
        else if canMove currentPlayer thePile
                then makeMove currentPlayer
                else cantMove currentPlayer
    game <- get
    (when (inPlay game) nextMove)


makeMove player = do
    str <- lift $ askMove (name player) 
    let cardsToPlay = getCards player (indexesFromString str)

    currentPile <- gets pile
    if not (validMove (head cardsToPlay) currentPile) || not (allRanksSame cardsToPlay)
        then do 
            lift $ showBadMove cardsToPlay
            makeMove player
        else do
            layCardsST player cardsToPlay
            dealToHandST player (length cardsToPlay)

cantMove player = do
    lift $ pickUpWait (name player)
    pickUpPileST player

moveFromFaceDown player = do
    thePile <- gets pile
    cardToPlay <- lift $ askFaceDown (name player)
    let card = getCard player (cardToPlay-1)
    if validMove card thePile
       then do
           lift $ waitChoiceOk card
           layCardsST player [card]
       else do
           lift $ waitChoiceFail card
           pickUpPileST player
           pickUpFromFaceDownST player card     
