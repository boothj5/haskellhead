import System.IO.Unsafe         (unsafePerformIO)
import Control.Monad
import Data.Char
import Data.IORef

-- Card type
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
            deriving (Show, Eq, Ord, Enum)

data Suit = Hearts | Clubs | Diamonds | Spades deriving (Show, Eq)

data Card = Card Rank Suit deriving (Eq)

instance Show Card where
    show (Card rank suit) = show rank ++ " of " ++ show suit

-- player type
data Player = Player { name :: String
                       , hand :: [Card]
                       , faceUp :: [Card]
                       , faceDown :: [Card] 
                     } 

instance Show Player where
    show (Player {name=n, hand=h, faceUp=u, faceDown=d}) = "\nplayer name: " ++ n
                                                           ++ "\nhand: " ++ show h
                                                           ++ "\nfaceUp: " ++ show u
                                                           ++ "\nfaceDown: " ++ show d

-- Game state type
data GameDetails = GameDetails {
        numPlayers      :: !Int
       ,players         :: ![String]
       ,numCardsEach    :: !Int
       ,deck            :: ![Card]
    } 

instance Show GameDetails where
    show GameDetails { numPlayers   = n
                      ,players      = p
                      ,numCardsEach = c
                      ,deck         = d
                     } =  "\nGame Details: " 
                          ++ "\nPlayers: " ++ show n
                          ++ "\nNames: " ++ show p
                          ++ "\nCards Each: " ++ show c
                          ++ "\nDeck : " ++ show d
-- Initial state
emptySt :: GameDetails
emptySt = GameDetails {
        numPlayers      = 0
       ,players         = []
       ,numCardsEach    = 0
       ,deck           = []
    }

-- Global state variable
state :: IORef GameDetails
state = unsafePerformIO $ newIORef emptySt
{-# NOINLINE state #-}

-- Access a component of the state with a projection function
getGameProperty :: (GameDetails -> a) -> IO a
getGameProperty f = withGame (return . f)
   
-- Perform a (read-only) IO action on the state
withGame :: (GameDetails -> IO a) -> IO a
withGame f = readIORef state >>= f

-- Modify the game
modifyGame :: (GameDetails -> GameDetails) -> IO ()
modifyGame  f = modifyIORef state f

-- game functions
numDecksRequired :: (Integral t, Integral a) => a -> a -> t
numDecksRequired cs ps = ( div52 $ fromIntegral $ total cs ps ) + ( remDeck $ total cs ps )
    where div52 n   = truncate $ n / 52
          remDeck n = if n `mod` 52 > 0 then 1 else 0
          total n m = n * m * 3
          
getNewDeckWithEnoughCards :: Int -> [Card]
getNewDeckWithEnoughCards 0 = []
getNewDeckWithEnoughCards 1 = getNewDeck
getNewDeckWithEnoughCards n = getNewDeck ++ (getNewDeckWithEnoughCards $ n-1)

-- Some simple values
getNewDeck :: [Card]
getNewDeck = [Card rank suit | suit <- [Hearts, Clubs, Diamonds, Spades], rank <- [Two .. Ace]]

jamesHand :: [Card]
jamesHand = [Card Three Diamonds, Card Two Hearts]

jamesFaceUp :: [Card]
jamesFaceUp = [Card Ace Spades, Card Four Hearts]

jamesFaceDown :: [Card]
jamesFaceDown = [Card Seven Clubs, Card Ten Diamonds]

james = Player {name="James", hand=jamesHand, faceUp=jamesFaceUp, faceDown=jamesFaceDown}

getGameInfo = do
    putStrLn "Enter number of players:"
    players <- fmap read getLine
    putStrLn "Enter number of cards per hand:"
    cards <- fmap read getLine

    let newDeck = getNewDeckWithEnoughCards $ numDecksRequired cards players
    modifyGame $ \st ->
                    st { numPlayers     = players
                        ,numCardsEach   = cards
                        ,deck           = newDeck 
                       } 

getPlayerNames = do   
    n <- getGameProperty numPlayers
    playerNames <- forM [1..n] (\a -> do  
        putStrLn $ "Enter name for player " ++ show a ++ ":"  
        playerName <- getLine  
        return playerName)  
    modifyGame $ \st -> st { players = playerNames } 

showGame = do
    game <- readIORef state
    putStrLn $ show game

clearScreen = do
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

main = do
    startState <- readIORef state
    clearScreen
    putStrLn "Welcome to Haskellhead!"
    putStrLn ""
    putStrLn $ show startState
    putStrLn ""
    getGameInfo
    putStrLn ""
    getPlayerNames

--    withGame $ \st -> do
--        case numCardsEach st > 3 of
--            True -> putStrLn "More than 3 cards"   
--            False -> putStrLn "Less than or 3 cards"
    showGame
    
    
            
