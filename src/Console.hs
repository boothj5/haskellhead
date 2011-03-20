module Console where

clearScreen = do
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

welcome = do
    putStrLn "Welcome to Haskellhead!"
    
line = do
    putStrLn ""
    
getNumPlayers :: IO Int
getNumPlayers = do
    putStrLn "How many players?"
    nplayers <- fmap read getLine
    return nplayers
    
getNumCards :: IO Int    
getNumCards = do
    putStrLn "How many cards each?"
    cards <- fmap read getLine
    return cards
    
getPlayerName :: Int -> IO String
getPlayerName num = do
    putStrLn $ "Enter name for player " ++ show num ++ ":"  
    name <- getLine
    return name
    
showCardsDealt = do
    putStrLn "Cards dealt, press enter:"

waitUser = do
    getLine
    
showShitheadError = do
    putStrLn "ERROR - NO SHITHEAD :/"
    
showShithead name = do    
    putStrLn $ show name ++ " IS A SHITHEAD!!!!!!!!!!!!"
    
showGameDetails game = do
    (print game)
    
getSwapHand name = do
    putStrLn $ name ++ ", select a hand card to swap:"
    c <- fmap read getLine
    return c

getSwapFaceUp name = do
    putStrLn $ name ++ ", select a face up card to swap:"
    c <- fmap read getLine
    return c
    
showPlayer p = do
    (print p)
    
askSwapMore name = do
    putStrLn $ name ++ ", do you want to swap more cards?"
    swapMore <- getLine
    return swapMore

askSwap name = do
    putStrLn $ name ++ ", do you want to swap cards?"
    swap <- getLine
    return swap

showMove name cards = do
    putStrLn $ name ++ " laid the " ++ show cards

askMove name = do
    putStrLn $ name ++ ", which cards do you wish to lay?"
    str <- getLine
    return str
    
showBadMove cardsToPlay = do
    putStrLn $ "You cannot lay " ++ show cardsToPlay

pickUpWait name = do
    putStrLn $ "OH DEAR! " ++ name ++ ", you cannot move."
    putStrLn "Press enter to pick up the pile."
    getLine

askFaceDown name = do
    putStrLn $ name ++ ", which card do you wish choose?"
    cardToPlay <- fmap read getLine
    return cardToPlay

waitChoiceOk card = do
    putStrLn $ "Whew you chose the " ++ show card ++ ", press enter,"
    getLine

waitChoiceFail card = do
    putStrLn $ "OH DEAR! You chose the " ++ show card ++ ", press enter,"
    getLine

