import System.IO.Unsafe         (unsafePerformIO)
import Data.Char
import Data.IORef

-- Rank, Suit and Card types
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
data GameState = GameState {
        numPlayers      :: !Int
       ,numCardsEach    :: !Int
       ,deck            :: ![Card]
    } deriving (Show)

-- The initial state
emptySt :: GameState
emptySt = GameState {
        numPlayers      = 0
       ,numCardsEach    = 0
       ,deck           = []
    }

-- Global state
state :: IORef GameState
state = unsafePerformIO $ newIORef emptySt
{-# NOINLINE state #-}

-- Access a component of the state with a projection function
getGameProperty :: (GameState -> a) -> IO a
getGameProperty f = withGame (return . f)
   
-- Perform a (read-only) IO action on the state
withGame :: (GameState -> IO a) -> IO a
withGame f = readIORef state >>= f

modifyGame :: (GameState -> GameState) -> IO ()
modifyGame  f = modifyIORef state f

-- game functions
totalCardsNeeded :: (Num a) => a -> a -> a
totalCardsNeeded cs ps = cs * ps * 3

divided :: (RealFrac a, Integral b) => a -> b
divided cs = truncate $ cs / 52

additional :: (Integral a, Num t) => a -> t
additional cs | cs `mod` 52 > 0 = 1
       | otherwise       = 0  

numDecksRequired :: (Integral t, Integral a) => a -> a -> t
numDecksRequired cs ps = ( divided $ fromIntegral $ totalCardsNeeded cs ps ) + ( additional $ totalCardsNeeded cs ps )

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

    newState <- readIORef state
    putStrLn $ show newState

    let decks = numDecksRequired cards players
        newDeck = getNewDeckWithEnoughCards decks
    putStrLn $ "Number of decks: " ++ show decks
    
    modifyGame $ \st ->
                    st { numPlayers     = players
                        ,numCardsEach   = cards
                        ,deck           = newDeck 
                       } 

showDeck = do
    putStrLn "Deck:"
    print getNewDeck

showPlayers = do 
    print james

showGame = do
    game <- readIORef state
    putStrLn $ show game

main = do
    startState <- readIORef state

    putStrLn "Haskell head"
    putStrLn ""

    putStrLn $ show startState
    putStrLn ""

    getGameInfo

    showDeck
    showPlayers
    
    putStrLn ""
    
    withGame $ \st -> do
        case numCardsEach st > 3 of
            True -> putStrLn "More than 3 cards"   
            False -> putStrLn "Less than or 3 cards"
            
    showGame
    
    
            
