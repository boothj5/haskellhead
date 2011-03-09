module State 
( getGameST
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
, pickUpFromFaceDownST
) where

import System.IO.Unsafe         (unsafePerformIO)
import Data.IORef
import Data.Maybe
import Control.Monad
import System.Random
import System.Random.Shuffle
import Game
import Player
   
------------------------------------------------

--
-- Basic state management
--

-- Initial state
emptyST :: Game
emptyST = Game { numPlayers      = 0
               , players         = []
               , numCardsEach    = 0
               , deck            = [] 
               , pile            = []
               , burnt           = []
               , lastMove        = "" }

-- Global state variable
stateST :: IORef Game
stateST = unsafePerformIO $ newIORef emptyST
{-# NOINLINE stateST #-}
   
-- Access a component of the state with a projection function
getGamePropertyST :: (Game -> a) -> IO a
getGamePropertyST f = withGameST (return . f)
   
-- Perform a (read-only) IO action on the state
withGameST :: (Game -> IO a) -> IO a
withGameST f = readIORef stateST >>= f

-- Modify the game state
modifyGameST :: (Game -> Game) -> IO ()
modifyGameST = modifyIORef stateST

------------------------------------------------

--
-- Functions that manupulate the game state
--

-- get the game state
getGameST = readIORef stateST
   
-- create the deck of correct size
createDeckST ncards nplayers = do
    gen <- getStdGen
    let newDeck = newDeckWithEnoughCards ncards nplayers
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
    let swappedPlayers = swapForPlayer player playerList cardFromHand cardFromFaceUp
    modifyGameST $ \st -> st { players = swappedPlayers }

-- Lay the cards from the players hand
layCardsST player cards = do
    ps <- getGamePropertyST players
    p <- getGamePropertyST pile
    let newPile = cards ++ p
        nPlayerList = removeCardsFromPlayer player ps cards
        nPlayerList2 = makeCurrentPlayer player nPlayerList
        move = name player ++ " laid the " ++ show cards

    modifyGameST $ \st -> 
                    st { pile    = newPile 
                        ,players = nPlayerList2
                        ,lastMove = move }
    burnST
    missAGoST
    
-- burn the pile if a burn card or four of a kind are on the top
burnST = do
    cs <- getGamePropertyST pile
    bcs <- getGamePropertyST burnt
    ps <- getGamePropertyST players
    let nPile = burn cs
        nBurnt = if null nPile then cs ++ bcs else bcs
        nPlayers = if null nPile then last ps:init ps else ps
        
    modifyGameST $ \st -> st { pile    = nPile
                              ,burnt   = nBurnt
                              ,players = nPlayers }

-- skip the next player if miss a go card was played
missAGoST = do
    cs <- getGamePropertyST pile
    ps <- getGamePropertyST players
    (when (missAGo cs) $ do
            let newPs = nextTurn ps
            modifyGameST $ \st -> st { players = newPs })

-- make player pick up pile
pickUpPileST player = do
    cs <- getGamePropertyST pile
    ps <- getGamePropertyST players
    let pickedUpPs = addToPlayersHand player ps cs
        move = name player ++ " picked up " ++ show (length cs) ++ " cards"
    modifyGameST $ \st -> st { players = pickedUpPs, pile = [], lastMove = move }

-- make player pick up chosen card from their face down hand
pickUpFromFaceDownST player card = do
    ps <- getGamePropertyST players
    let pickedUpPs = addToPlayersHand player ps [card]
        pickedUpPs2 = removeFromPlayersFaceDown player pickedUpPs [card] 
    modifyGameST $ \st -> st { players = pickedUpPs2 }

-- move on to next player
moveToNextPlayerST = do
    ps <- getGamePropertyST players
    let newPs = moveToNextPlayerWithCards ps
    modifyGameST $ \st -> st { players = newPs }
    return (head newPs)

-- deal a card from the deck to the players hand
dealToHandST player num = do
    cs <- getGamePropertyST deck
    ps <- getGamePropertyST players
    n <- getGamePropertyST numCardsEach
    
    let sizeOfHand = length (hand (   fromJust (getPlayer (name player) ps)    )    )
        numToDeal | sizeOfHand >= n = 0
                  | otherwise = n - sizeOfHand
    if numToDeal == 0
       then return ()
       else do
            let dealtPs = addToPlayersHand player ps (take numToDeal cs)
            modifyGameST $ \st -> st { players = dealtPs, deck = drop numToDeal cs }

-- deal a card from the deck to the players face up hand
dealToFaceUpST p = do
    cs <- getGamePropertyST deck
    ps <- getGamePropertyST players
    let dealtPs = addToPlayersFaceUp p ps (head cs)
    modifyGameST $ \st -> st { players = dealtPs, deck = tail cs }

-- deal a card from the deck to the players face down hand
dealToFaceDownST p = do
    cs <- getGamePropertyST deck
    ps <- getGamePropertyST players
    let dealtPs = addToPlayersFaceDown p ps (head cs)
    modifyGameST $ \st -> st { players = dealtPs, deck = tail cs }

-- deal the cards to the players
dealST = do
    cardsEach  <- getGamePropertyST numCardsEach
    playerList <- getGamePropertyST players
    forM playerList (\p -> 
        forM [1..cardsEach] (\_ -> do
            dealToFaceDownST p
            dealToFaceUpST p 
            dealToHandST p 1))
