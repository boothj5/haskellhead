-- | Module for interacting with the user via the terminal
module Console 
( console
, clearScreen
, welcome
, line
, getNumPlayers
, getNumCards
, getPlayerName
, showCardsDealt
, waitUser
, showShitheadError
, showShithead
, showGameDetails
, getSwapHand
, getSwapFaceUp 
, showPlayer
, askSwapMore
, askSwap
, showMove
, askMove
, showBadMove
, pickUpWait
, askFaceDown
, waitChoiceOk
, waitChoiceFail
) where

import Control.Monad.State

-- | Just performs a lift to allow IO whilst modifying the game state
console :: (MonadTrans t, Monad m) => m a -> t m a
console = lift

-- | Clear the terminal
clearScreen :: IO ()
clearScreen = do
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

-- | Show welcome message
welcome :: IO ()
welcome = putStrLn "Welcome to Haskellhead!"
    
-- | Print a blank line
line :: IO ()
line = putStrLn ""
    
-- | Request the number of players in the game
getNumPlayers :: IO Int
getNumPlayers = do
    putStrLn "How many players?"
    fmap read getLine
    
-- | Request the number of cards in each players hand
getNumCards :: IO Int    
getNumCards = do
    putStrLn "How many cards each?"
    fmap read getLine
    
-- | Given a players position in the game, request their name
getPlayerName :: Int -> IO String
getPlayerName num = do
    putStrLn $ concat [ "Enter name for player "
                      , show num
                      , ":" ]  
    getLine

-- | Show cards dealt message
showCardsDealt :: IO ()
showCardsDealt = putStrLn "Cards dealt, press enter:"

-- | Wait on user
waitUser :: IO String
waitUser = getLine
    
-- | Show error when no shithead found
showShitheadError :: IO ()
showShitheadError = putStrLn "ERROR - NO SHITHEAD :/"
    
-- | Show the shithead
showShithead :: (Show a) => a -> IO ()
showShithead name = 
    putStrLn $ concat [ show name
                      , " IS A SHITHEAD!!!!!!!!!!!!" ]
    
-- | Show all game details
showGameDetails :: (Show a) => a -> IO ()
showGameDetails = print
    
-- | Get card to swap from hand
getSwapHand :: (Read b) => String -> IO b
getSwapHand name = do
    putStrLn $ concat [ name
                      , ", select a hand card to swap:" ]
    fmap read getLine

-- | Get card to swap from face up pile
getSwapFaceUp :: (Read b) => String -> IO b
getSwapFaceUp name = do
    putStrLn $ concat [ name
                      , ", select a face up card to swap:" ]
    fmap read getLine
    
-- | Show player details
showPlayer :: (Show a) => a -> IO ()
showPlayer = print
    
-- | Ask if the player wishes to swap more cards
askSwapMore :: String -> IO String
askSwapMore name = do
    putStrLn $ concat [ name
                      , ", do you want to swap more cards?" ]
    getLine

-- | Ask if the player wishes to swap cards
askSwap :: String -> IO String
askSwap name = do
    putStrLn $ concat [ name
                      , ", do you want to swap cards?" ]
    getLine

-- | Show who made the first move
showMove :: (Show a) => String -> a -> IO ()
showMove name cards = 
    putStrLn $ concat [ name
                      , " laid the "
                      , show cards ]

-- | Ask the player to choose some cards to lay
askMove :: String -> IO String
askMove name = do
    putStrLn $ concat [ name
                      , ", which cards do you wish to lay?" ]
    getLine
    
-- | Show message when player chose invalid cards
showBadMove :: (Show a) => a -> IO ()
showBadMove cardsToPlay = 
    putStrLn $ concat [ "You cannot lay "
                      , show cardsToPlay ]

-- | Show a message and wait on user, when player cannot lay
pickUpWait :: String -> IO String
pickUpWait name = do
    putStrLn $ concat [ "OH DEAR! "
                      , name
                      , ", you cannot move." ]
    putStrLn "Press enter to pick up the pile."
    getLine

-- | Ask the player to choose a face down card
askFaceDown :: (Read b) => String -> IO b
askFaceDown name = do
    putStrLn $ concat [ name
                      , ", which card do you wish choose?" ]
    fmap read getLine

-- | Face down card was layable, show message and wait on user
waitChoiceOk :: (Show a) => a -> IO String
waitChoiceOk card = do
    putStrLn $ concat [ "Whew you chose the "
                      , show card
                      , ", press enter," ]
    getLine

-- | Face down card was not layable, show message and wait on user
waitChoiceFail :: (Show a) => a -> IO String
waitChoiceFail card = do
    putStrLn $ concat [ "OH DEAR! You chose the "
                      , show card
                      , ", press enter," ]
    getLine

