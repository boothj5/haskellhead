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
        numPlayers     :: !Int
       ,numCardsEach   :: !Int
    } deriving (Show)

-- The initial state
emptySt :: GameState
emptySt = GameState {
        numPlayers      = 0
       ,numCardsEach    = 0
    }

-- Global state
state :: IORef GameState
state = unsafePerformIO $ newIORef emptySt
{-# NOINLINE state #-}

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

-- Some simple values
deck :: [Card]
deck = [Card rank suit | suit <- [Hearts, Clubs, Diamonds, Spades], rank <- [Two .. Ace]]

jamesHand :: [Card]
jamesHand = [Card Three Diamonds, Card Two Hearts]

jamesFaceUp :: [Card]
jamesFaceUp = [Card Ace Spades, Card Four Hearts]

jamesFaceDown :: [Card]
jamesFaceDown = [Card Seven Clubs, Card Ten Diamonds]

james = Player {name="James", hand=jamesHand, faceUp=jamesFaceUp, faceDown=jamesFaceDown}

getGameInfo = do

    putStrLn "Enter number of players:"
    enteredPlayers <- fmap read getLine
  
    putStrLn "Enter number of cards per hand:"
    enteredCardsEach <- fmap read getLine

    writeIORef state GameState { numPlayers = enteredPlayers, numCardsEach = enteredCardsEach } 

    newState <- readIORef state
    putStrLn $ show newState

    let decks = numDecksRequired enteredCardsEach enteredPlayers
    putStrLn $ "Number of decks: " ++ show decks

showDeck = do
    putStrLn "Deck:"
    print deck

showPlayers = do 
    print james

main = do
    startState <- readIORef state

    putStrLn "Haskell head"
    putStrLn ""

    putStrLn $ show startState
    putStrLn ""

    getGameInfo

    showDeck
    showPlayers



