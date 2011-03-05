module State 
( getGameDetailsST
, getGamePropertyST
, createDeckST
, createPlayersST
, swapCardsST
, layCardsST
, dealToHandST
, dealToFaceUpST
, dealToFaceDownST
, dealST
, moveToNextPlayerST
, pickUpPileST
) where

import System.IO.Unsafe         (unsafePerformIO)
import Data.IORef
import Control.Monad
import System.Random
import System.Random.Shuffle
import Game
   
------------------------------------------------

--
-- Basic state management
--

-- Initial state
emptyST :: GameDetails
emptyST = GameDetails { numPlayers      = 0
                       ,players         = []
                       ,numCardsEach    = 0
                       ,deck            = [] 
                       ,pile            = []
                       ,lastMove        = "" }

-- Global state variable
stateST :: IORef GameDetails
stateST = unsafePerformIO $ newIORef emptyST
{-# NOINLINE stateST #-}
   
-- Access a component of the state with a projection function
getGamePropertyST :: (GameDetails -> a) -> IO a
getGamePropertyST f = withGameST (return . f)
   
-- Perform a (read-only) IO action on the state
withGameST :: (GameDetails -> IO a) -> IO a
withGameST f = readIORef stateST >>= f

-- Modify the game state
modifyGameST :: (GameDetails -> GameDetails) -> IO ()
modifyGameST  f = modifyIORef stateST f

------------------------------------------------

--
-- Functions that manupulate the game state
--

-- get the game state
getGameDetailsST = do
    readIORef stateST
   
-- create the deck of correct size
createDeckST ncards nplayers = do
    gen <- getStdGen
    let newDeck = newDeckWithEnoughCards $ numDecksRequired ncards nplayers
        shuffledDeck = shuffle' newDeck (length newDeck) gen
    modifyGameST $ \st ->
                    st { numPlayers     = nplayers
                        ,numCardsEach   = ncards
                        ,deck           = shuffledDeck }
   
-- create the players
createPlayersST names = do
    let newPlayers = createPlayers names
    modifyGameST $ \st -> st { players = newPlayers } 
   
-- swap cards between players face up hand and face down hand
swapCardsST player cardFromHand cardFromFaceUp = do
    playerList <- getGamePropertyST players
    let swappedPlayers = swapForNamedPlayer player playerList cardFromHand cardFromFaceUp
    modifyGameST $ \st -> st { players = swappedPlayers }


-- Lay the cards from the players hand
layCardsST player cards = do
    ps <- getGamePropertyST players
    p <- getGamePropertyST pile
    let newPile = cards ++ p
        nPlayerList = removeFromNamedPlayersHand player ps cards
        nPlayerList2 = makeCurrentPlayer player nPlayerList
        move = (name player) ++ " laid the " ++ show cards
    modifyGameST $ \st -> 
                    st { pile    = newPile 
                        ,players = nPlayerList2
                        ,lastMove = move }

-- make player pick up pile
pickUpPileST player = do
    cs <- getGamePropertyST pile
    ps <- getGamePropertyST players
    let pickedUpPs = addToNamedPlayersHand player ps cs
        move = (name player) ++ " picked up " ++ (show $ length cs) ++ " cards"
    modifyGameST $ \st -> st { players = pickedUpPs, pile = [], lastMove = move }

-- move on to next player
moveToNextPlayerST = do
    ps <- getGamePropertyST players
    let newPs = nextTurn ps
    modifyGameST $ \st -> st { players = newPs }
    return (head newPs)

-- deal a card from the deck to the players hand
dealToHandST player num = do
    cs <- getGamePropertyST deck
    ps <- getGamePropertyST players
    let dealtPs = addToNamedPlayersHand player ps (take num cs)
    modifyGameST $ \st -> st { players = dealtPs, deck = (drop num cs) }

-- deal a card from the deck to the players face up hand
dealToFaceUpST p = do
    cs <- getGamePropertyST deck
    ps <- getGamePropertyST players
    let dealtPs = addToNamedPlayersFaceUp p ps (head cs)
    modifyGameST $ \st -> st { players = dealtPs, deck = (tail cs) }

-- deal a card from the deck to the players face down hand
dealToFaceDownST p = do
    cs <- getGamePropertyST deck
    ps <- getGamePropertyST players
    let dealtPs = addToNamedPlayersFaceDown p ps (head cs)
    modifyGameST $ \st -> st { players = dealtPs, deck = (tail cs) }

-- deal the cards to the players
dealST = do
    cardsEach  <- getGamePropertyST numCardsEach
    playerList <- getGamePropertyST players
    forM playerList (\p -> do
        forM [1..cardsEach] (\_ -> do
            dealToFaceDownST p
            dealToFaceUpST p 
            dealToHandST p 1))
