-- | Main module
module Main where

import Data.IORef
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Char

import Card
import HumanPlayer
import Game
import State
import Console
import Util

-- | main just calls evalStateT on startGame passing a new empty game
main :: IO ()
main = evalStateT startGame newGameST

-- | contains the flow of the game, the 'engine'
startGame :: StateT Game IO ()
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
    
    endGame
    
-- | Show all game details
showGame :: StateT Game IO ()
showGame = do
    game <- get
    console $ showGameDetails game

-- | Get names of players from user and create them
getPlayerNames :: StateT Game IO ()
getPlayerNames = do   
    n <- gets numPlayers
    playerNames <- forM [1..n] (console . getPlayerName)  
    createPlayersST playerNames 

-- | Ask a player to swap cards between their hand and face up pile
doSwap :: (MonadState Game (t IO), MonadTrans t) => HumanPlayer -> t IO ()
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

-- | Ask each player whether they wish to swap cards
-- between their hand and face up pile
swapAll :: StateT Game IO [()]
swapAll = do
    playerList <- gets players
    forM playerList (\player -> do
        console clearScreen
        console (print player)
        let theName = name player
        swap <- console $ askSwap theName
        (when (charToBoolean swap) $ 
            doSwap player))
   
-- | Make the first move by finding the player with the 
-- lowest cards
makeFirstMove :: StateT Game IO ()
makeFirstMove = do
    playerList <- gets players
    let player = fromJust $ playerWithLowestCardFromCircle playerList
        cards = lowestCards player    
    layCardsST player cards
    dealToHandST player (length cards)
    console $ showMove (name player) cards

-- | The main game play, go through players asking them to play
-- until there is only one player left
nextMove :: StateT Game IO ()
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

-- | Get a specific player to make a move
makeMove :: (MonadState Game (t IO), MonadTrans t) => HumanPlayer -> t IO ()
makeMove player = do
    str <- console $ askMove (name player) 
    let cardsToPlay = getCards player (indexesFromString str)

    currentPile <- gets pile
    if not (validMove (head cardsToPlay) currentPile) || not (allRanksEqual cardsToPlay)
        then do 
            console $ showBadMove cardsToPlay
            makeMove player
        else do
            layCardsST player cardsToPlay
            dealToHandST player (length cardsToPlay)

-- | When the player cannot move, make them pick up
cantMove :: (MonadState Game (t IO), MonadTrans t) => HumanPlayer -> t IO ()
cantMove player = do
    console $ pickUpWait (name player)
    pickUpPileST player

-- | Get the player to choose a card from their face down pile
moveFromFaceDown :: (MonadState Game (t IO), MonadTrans t) => HumanPlayer -> t IO ()
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

-- | Find a show the shithead
endGame :: StateT Game IO ()
endGame = do
    playerList <- gets players
    let shithead = getShithead playerList 
    case shithead of
       Nothing -> console showShitheadError
       _       -> console $ showShithead (name $ fromJust shithead)