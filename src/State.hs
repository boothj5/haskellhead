module State where

import System.IO.Unsafe         (unsafePerformIO)
import Data.IORef
import Control.Monad
import Game
   
------------------------------------------------

--
-- Game stateST management
--

-- Initial stateST
emptyST :: GameDetails
emptyST = GameDetails { numPlayers      = 0
                       ,players         = []
                       ,numCardsEach    = 0
                       ,deck            = [] 
                       ,pile            = [] }

-- Global stateST variables
stateST :: IORef GameDetails
stateST = unsafePerformIO $ newIORef emptyST
{-# NOINLINE stateST #-}
   
-- Access a component of the stateST with a projection function
getGamePropertyST :: (GameDetails -> a) -> IO a
getGamePropertyST f = withGameST (return . f)
   
-- Perform a (read-only) IO action on the stateST
withGameST :: (GameDetails -> IO a) -> IO a
withGameST f = readIORef stateST >>= f

-- Modify the game
modifyGameST :: (GameDetails -> GameDetails) -> IO ()
modifyGameST  f = modifyIORef stateST f


dealToHandST p = do
    cs <- getGamePropertyST deck
    ps <- getGamePropertyST players

    let dealtPs = addToNamedPlayersHand p ps (head cs)
    modifyGameST $ \st -> st { players = dealtPs, deck = (tail cs) }

dealToFaceUpST p = do
    cs <- getGamePropertyST deck
    ps <- getGamePropertyST players

    let dealtPs = addToNamedPlayersFaceUp p ps (head cs)
    modifyGameST $ \st -> st { players = dealtPs, deck = (tail cs) }

dealToFaceDownST p = do
    cs <- getGamePropertyST deck
    ps <- getGamePropertyST players

    let dealtPs = addToNamedPlayersFaceDown p ps (head cs)
    modifyGameST $ \st -> st { players = dealtPs, deck = (tail cs) }

dealST = do
    cardsEach  <- getGamePropertyST numCardsEach
    playerList <- getGamePropertyST players

    forM playerList (\p -> do
        forM [1..cardsEach] (\_ -> do
            dealToFaceDownST p
            dealToFaceUpST p 
            dealToHandST p ))
