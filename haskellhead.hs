import System.IO.Unsafe         (unsafePerformIO)
import Control.Monad
import Data.Char
import Data.IORef

------------------------------------------------

--
-- Data types
--

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
            deriving (Show, Eq, Ord, Enum)
data Suit = Hearts | Clubs | Diamonds | Spades deriving (Show, Eq, Ord)
data Card = Card Rank Suit deriving (Eq, Ord)
instance Show Card where
    show (Card rank suit) = show rank ++ " of " ++ show suit

data Player = Player { 
                name         :: String
               ,hand         :: [Card]
               ,faceUp       :: [Card]
               ,faceDown     :: [Card] }
instance Show Player where
    show (Player {name=n, hand=h, faceUp=u, faceDown=d}) = "\nplayer name: " ++ n
                                                           ++ "\nhand: " ++ show h
                                                           ++ "\nfaceUp: " ++ show u
                                                           ++ "\nfaceDown: " ++ show d
instance Eq Player where
    p1 == p2 = (name p1) == (name p2)

-- Used to represent the state of the game
data GameDetails = GameDetails { numPlayers      :: !Int
                                ,players         :: ![Player]
                                ,numCardsEach    :: !Int
                                ,deck            :: ![Card]
                                ,pile            :: ![Card]
                               } 
instance Show GameDetails where
    show GameDetails { numPlayers   = n
                      ,players      = p
                      ,numCardsEach = c
                      ,deck         = d 
                      ,pile         = pile } =  "\nGame Details: " 
                          ++ "\nPlayers: " ++ show n
                          ++ "\nCards Each: " ++ show c
                          ++ "\n"
                          ++ "\nPlayers details: " ++ show p
                          ++ "\n" 
                          ++ "\nDeck : " ++ show d
                          ++ "\nPile : " ++ show pile

------------------------------------------------

--
-- Game state management
--

-- Initial state
emptySt :: GameDetails
emptySt = GameDetails { numPlayers      = 0
                       ,players         = []
                       ,numCardsEach    = 0
                       ,deck            = [] 
                       ,pile            = [] }

-- Global state variables
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

------------------------------------------------
--
-- game functions
--

-- determine the number of decks of cards required in game, 
-- given number of players and number of cards per hand
numDecksRequired :: (Integral t, Integral a) => a -> a -> t
numDecksRequired cs ps = ( div52 $ fromIntegral $ total cs ps ) + ( remDeck $ total cs ps )
    where div52 n   = truncate $ n / 52
          remDeck n = if n `mod` 52 > 0 then 1 else 0
          total n m = n * m * 3
          
-- return a new unshuffled deck of cards
newDeck :: [Card]
newDeck = [Card rank suit | suit <- [Hearts, Clubs, Diamonds, Spades], rank <- [Two .. Ace]]

-- returns a number of decks as one, i.e. with two decks, 
-- every card will be represented twice
newDeckWithEnoughCards :: Int -> [Card]
newDeckWithEnoughCards 0 = []
newDeckWithEnoughCards 1 = newDeck
newDeckWithEnoughCards n = newDeck ++ (newDeckWithEnoughCards $ n-1)

-- Given a list of names will return a list of players with those names
createPlayers :: [String] -> [Player]
createPlayers [] = []
createPlayers (x:[]) = ( Player { name = x, hand = [], faceUp = [], faceDown = []} ) : []
createPlayers (x:xs) = ( Player { name = x, hand = [], faceUp = [], faceDown = []} ) : createPlayers xs

-- Given a player and a card, will return a new player, 
-- with every thing the same but the Card added to one of their hands
addToPlayersHand :: Player -> Card -> Player
addToPlayersHand p c = Player { name        = ( name p )
                               ,hand        = ( c : (hand p) )
                               ,faceUp      = ( faceUp p )
                               ,faceDown    = ( faceDown p ) }

addToPlayersFaceUp :: Player -> Card -> Player
addToPlayersFaceUp p c = Player { name      = ( name p )
                                 ,hand      = ( hand p )
                                 ,faceUp    = ( c : (faceUp p) )
                                 ,faceDown  = ( faceDown p ) }


addToPlayersFaceDown :: Player -> Card -> Player
addToPlayersFaceDown p c = Player { name        = ( name p )
                                   ,hand        = ( hand p )
                                   ,faceUp      = ( faceUp p )
                                   ,faceDown    = ( c : (faceDown p) ) }
                                   
-- Given a player, a list of players, and a card, returns
-- a list of players with everything the same but the card
-- added to one of the players hands, whos name matches that of the player 
-- passed in
addToNamedPlayersHand :: Player -> [Player] -> Card -> [Player]
addToNamedPlayersHand _ []     _   = []
addToNamedPlayersHand p1 (p2:ps) c | p1 == p2    = (addToPlayersHand p2 c) : ps
                                   | otherwise   = p2 : (addToNamedPlayersHand p1 ps c)

addToNamedPlayersFaceUp :: Player -> [Player] -> Card -> [Player]
addToNamedPlayersFaceUp _ []     _   = []
addToNamedPlayersFaceUp p1 (p2:ps) c | p1 == p2    = (addToPlayersFaceUp p2 c) : ps
                                     | otherwise   = p2 : (addToNamedPlayersFaceUp p1 ps c)

addToNamedPlayersFaceDown :: Player -> [Player] -> Card -> [Player]
addToNamedPlayersFaceDown _ []     _   = []
addToNamedPlayersFaceDown p1 (p2:ps) c | p1 == p2    = (addToPlayersFaceDown p2 c) : ps
                                       | otherwise   = p2 : (addToNamedPlayersFaceDown p1 ps c)

swapHandWithFaceUp :: Player -> Int -> Int -> Player
swapHandWithFaceUp p h f = Player { name     = ( name p )
                                   ,hand     = (map (\c -> if (handCard == c) then faceUpCard else c) (hand p))
                                   ,faceUp   = (map (\c -> if (faceUpCard == c) then handCard else c) (faceUp p))
                                   ,faceDown = ( faceDown p ) }
    where handCard   = (hand p) !! h
          faceUpCard = (faceUp p) !! f
          
swapForNamedPlayer :: Player -> [Player] -> Int -> Int -> [Player]
swapForNamedPlayer p1 (p2:ps) h f | p1 == p2  = (swapHandWithFaceUp p2 h f) : ps
                                  | otherwise = p2 : (swapForNamedPlayer p1 ps h f)

charToBoolean :: String -> Bool
charToBoolean s | (toUpper $ s !! 0) == 'Y'= True
		| otherwise 	    	   = False
		
playerWithLowestCard :: Player -> Player -> Player
playerWithLowestCard p1 p2 = if ((min p1Min p2Min) == p1Min) then p1 else p2
    where p1Min = minimum $ hand p1
          p2Min = minimum $ hand p2

playerWithLowestCardFromList :: [Player] -> Player
playerWithLowestCardFromList [] = error "No players"
playerWithLowestCardFromList (player:[]) = player
playerWithLowestCardFromList (player:rest) = playerWithLowestCard player (playerWithLowestCardFromList rest)

getLowestCards :: Player -> [Card]
getLowestCards p = (minimum $ hand p) : []

removeFromPlayersHand :: Player -> [Card] -> Player
removeFromPlayersHand p [] = p
removeFromPlayersHand p cs = Player { name = ( name p )
                                     ,hand = ( filter (\c -> c `notElem` cs) $ hand p )
                                     ,faceUp = ( faceUp p )
                                     ,faceDown = ( faceDown p ) }

removeFromNamedPlayersHand :: Player -> [Player] -> [Card] -> [Player]
removeFromNamedPlayersHand _ [] _        = []
removeFromNamedPlayersHand _ ps []       = ps
removeFromNamedPlayersHand p1 (p2:ps) cs | p1 == p2  = (removeFromPlayersHand p2 cs) : ps
                                         | otherwise = p2 : (removeFromNamedPlayersHand p1 ps cs)

------------------------------------------------
--
-- IO Actions, either manipulate the game state,
-- or interact with the user
--

clearScreen = do
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

showGame = do
    game <- readIORef state
    putStrLn $ show game

getGameInfo = do
    putStrLn "Enter number of players:"
    players <- fmap read getLine
    putStrLn "Enter number of cards per hand:"
    cards <- fmap read getLine

    newDeck <- return $ newDeckWithEnoughCards $ numDecksRequired cards players
    modifyGame $ \st ->
                    st { numPlayers     = players
                        ,numCardsEach   = cards
                        ,deck           = newDeck }
                       
getPlayerNames = do   
    n <- getGameProperty numPlayers
    
    playerNames <- forM [1..n] (\a -> do  
        putStrLn $ "Enter name for player " ++ show a ++ ":"  
        playerName <- getLine  
        return playerName)  
    
    newPlayers <- return $ createPlayers playerNames 
    modifyGame $ \st -> st { players = newPlayers } 

dealToHand p = do
    cs <- getGameProperty deck
    ps <- getGameProperty players

    dealtPs <- return $ addToNamedPlayersHand p ps (head cs)
    modifyGame $ \st -> st { players = dealtPs, deck = (tail cs) }

dealToFaceUp p = do
    cs <- getGameProperty deck
    ps <- getGameProperty players

    dealtPs <- return $ addToNamedPlayersFaceUp p ps (head cs)
    modifyGame $ \st -> st { players = dealtPs, deck = (tail cs) }

dealToFaceDown p = do
    cs <- getGameProperty deck
    ps <- getGameProperty players

    dealtPs <- return $ addToNamedPlayersFaceDown p ps (head cs)
    modifyGame $ \st -> st { players = dealtPs, deck = (tail cs) }

deal = do
    cardsEach  <- getGameProperty numCardsEach
    playerList <- getGameProperty players

    forM playerList (\p -> do
        forM [1..cardsEach] (\_ -> do
            dealToFaceDown p
            dealToFaceUp p 
            dealToHand p ))

doSwap playerList p = do
    theName <- return $ name p
    putStrLn $ theName ++ ", select a hand card to swap:"
    handCardToSwap   <- fmap read getLine
    putStrLn $ theName ++ ", select a face up card to swap:"
    faceUpCardToSwap <- fmap read getLine

    swappedPlayers <- return $ swapForNamedPlayer p playerList (handCardToSwap-1) (faceUpCardToSwap-1)
    modifyGame $ \st -> st { players = swappedPlayers }

    putStrLn $ theName ++ ", do you want to swap more cards?"
    swapMore <- getLine

    if (charToBoolean swapMore) 
        then
            doSwap playerList p
        else 
            return ()


swapAll = do
    playerList <- getGameProperty players

    forM playerList (\p -> do
        theName <- return $ name p

        putStrLn $ theName ++ ", do you want to swap cards?"
        swap <- getLine

        if (charToBoolean swap) 
            then 
                doSwap playerList p
            else 
                return ())
   
firstMove = do
    playerList <- getGameProperty players
    oPile <- getGameProperty pile

    p <- return $ playerWithLowestCardFromList playerList
    cs <- return $ getLowestCards p    
    nPile <- return $ cs ++ oPile
    nPlayerList <- return $ removeFromNamedPlayersHand p playerList cs

    modifyGame $ \st -> 
                    st { pile    = nPile 
                        ,players = nPlayerList }
    dealToHand p


main = do
    clearScreen
    putStrLn "Welcome to Haskellhead!"
    putStrLn ""

    getGameInfo
    putStrLn ""

    getPlayerNames

    deal

    clearScreen
    showGame
    putStrLn ""
    putStrLn "Press enter to continue"
    getLine

    swapAll

    firstMove

    showGame
    


--    withGame $ \st -> do
--        case numCardsEach st > 3 of
--            True -> putStrLn "More than 3 cards"   
--            False -> putStrLn "Less than or 3 cards"



            
