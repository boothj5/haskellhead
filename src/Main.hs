import Game
import Data.IORef
import Control.Monad
import System.Random
import System.Random.Shuffle

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

    gen <- getStdGen
    let newDeck = newDeckWithEnoughCards $ numDecksRequired cards players
        shuffledDeck = shuffle' newDeck (length newDeck) gen

    modifyGame $ \st ->
                    st { numPlayers     = players
                        ,numCardsEach   = cards
                        ,deck           = shuffledDeck }
                       
getPlayerNames = do   
    n <- getGameProperty numPlayers
    
    playerNames <- forM [1..n] (\a -> do  
        putStrLn $ "Enter name for player " ++ show a ++ ":"  
        playerName <- getLine  
        return playerName)  
    
    let newPlayers = createPlayers playerNames 
    modifyGame $ \st -> st { players = newPlayers } 

dealToHand p = do
    cs <- getGameProperty deck
    ps <- getGameProperty players

    let dealtPs = addToNamedPlayersHand p ps (head cs)
    modifyGame $ \st -> st { players = dealtPs, deck = (tail cs) }

dealToFaceUp p = do
    cs <- getGameProperty deck
    ps <- getGameProperty players

    let dealtPs = addToNamedPlayersFaceUp p ps (head cs)
    modifyGame $ \st -> st { players = dealtPs, deck = (tail cs) }

dealToFaceDown p = do
    cs <- getGameProperty deck
    ps <- getGameProperty players

    let dealtPs = addToNamedPlayersFaceDown p ps (head cs)
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
    let theName = name p
    putStrLn $ theName ++ ", select a hand card to swap:"
    handCardToSwap <- fmap read getLine
    putStrLn $ theName ++ ", select a face up card to swap:"
    faceUpCardToSwap <- fmap read getLine

    let swappedPlayers = swapForNamedPlayer p playerList (handCardToSwap-1) (faceUpCardToSwap-1)
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

    let p = playerWithLowestCardFromList playerList
        cs = getLowestCards p    
        nPile = cs ++ oPile
        nPlayerList = removeFromNamedPlayersHand p playerList cs
        nPlayerList2 = makeCurrentPlayer p nPlayerList

    modifyGame $ \st -> 
                    st { pile    = nPile 
                        ,players = nPlayerList2 }
    dealToHand p

    putStrLn $ show (name p) ++ " laid the " ++ show cs


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

