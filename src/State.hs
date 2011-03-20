-- | Module to make manupulations to the games state during game play
module State 
( newGameST
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

import Data.Maybe
import Control.Monad
import Control.Monad.State
import System.Random
import System.Random.Shuffle
import Game
import Player
import Card
   
-- | Initial empty game
newGameST :: Game
newGameST = Game { numPlayers      = 0
                 , players         = []
                 , numCardsEach    = 0
                 , deck            = [] 
                 , pile            = []
                 , burnt           = []
                 , lastMove        = "" }

-- | create the deck of correct size
createDeckST :: (MonadState Game (t IO), MonadTrans t) => Int -> Int -> t IO ()
createDeckST ncards nplayers = do
    gen <- lift getStdGen
    let newDeck = newDeckWithEnoughCards ncards nplayers
        shuffledDeck = shuffle' newDeck (length newDeck) gen
    modify $ \st ->
                    st { numPlayers     = nplayers
                        ,numCardsEach   = ncards
                        ,deck           = shuffledDeck }
   
-- | create the players
createPlayersST :: (MonadState Game m) => [String] -> m ()
createPlayersST names = do
    let newPlayers = createPlayers names
    modify $ \st -> st { players = newPlayers } 
   
-- | swap cards between players face up hand and face down hand
swapCardsST :: (MonadState Game m) => Player -> Int -> Int -> m ()
swapCardsST player cardFromHand cardFromFaceUp = do
    playerList <- gets players
    let swappedPlayers = swapForPlayer player playerList cardFromHand cardFromFaceUp
    modify $ \st -> st { players = swappedPlayers }

-- | Lay the cards from the players hand
layCardsST :: (MonadState Game m) => Player -> [Card] -> m ()
layCardsST player cards = do
    ps <- gets players
    p <- gets pile
    let newPile = cards ++ p
        nPlayerList = removeCardsFromPlayer player ps cards
        nPlayerList2 = makeCurrentPlayer player nPlayerList
        move = name player ++ " laid the " ++ show cards

    modify $ \st -> 
                    st { pile    = newPile 
                        ,players = nPlayerList2
                        ,lastMove = move }

    burnST
    missAGoST

-- | burn the pile if a burn card or four of a kind are on the top
burnST :: (MonadState Game m) => m ()
burnST = do
    cs <- gets pile
    bcs <- gets burnt
    ps <- gets players
    let nPile = burn cs
        nBurnt = if null nPile then cs ++ bcs else bcs
        nPlayers = if null nPile then last ps:init ps else ps
    modify $ \st -> st { pile    = nPile
                        ,burnt   = nBurnt
                        ,players = nPlayers }

-- | skip the next player if miss a go card was played
missAGoST :: (MonadState Game m) => m ()
missAGoST = do
    cs <- gets pile
    ps <- gets players
    (when (missAGo cs) $ do
            let newPs = nextTurn ps
            modify $ \st -> st { players = newPs })

-- | make player pick up pile
pickUpPileST :: (MonadState Game m) => Player -> m ()
pickUpPileST player = do
    cs <- gets pile
    ps <- gets players
    let pickedUpPs = addToPlayersHand player ps cs
        move = name player ++ " picked up " ++ show (length cs) ++ " cards"
    modify $ \st -> st { players = pickedUpPs, pile = [], lastMove = move }

-- | make player pick up chosen card from their face down hand
pickUpFromFaceDownST :: (MonadState Game m) => Player -> Card -> m ()
pickUpFromFaceDownST player card = do
    ps <- gets players
    let pickedUpPs = addToPlayersHand player ps [card]
        pickedUpPs2 = removeFromPlayersFaceDown player pickedUpPs [card] 
    modify $ \st -> st { players = pickedUpPs2 }

-- | move on to next player
moveToNextPlayerST :: (MonadState Game m) => m ()
moveToNextPlayerST = do
    ps <- gets players
    let newPs = moveToNextPlayerWithCards ps
    modify $ \st -> st { players = newPs }

-- | deal a card from the deck to the players hand
dealToHandST :: (MonadState Game m) => Player -> t -> m ()
dealToHandST player num = do
    cs <- gets deck
    ps <- gets players
    n <- gets numCardsEach
    
    let sizeOfHand = length (hand (   fromJust (getPlayer (name player) ps)    )    )
        numToDeal | sizeOfHand >= n = 0
                  | otherwise = n - sizeOfHand
    (unless (numToDeal == 0) $
       do let dealtPs = addToPlayersHand player ps (take numToDeal cs)
          modify $ \st -> st { players = dealtPs, deck = drop numToDeal cs })

-- | deal a card from the deck to the players face up hand
dealToFaceUpST :: (MonadState Game m) => Player -> m ()
dealToFaceUpST p = do
    cs <- gets deck
    ps <- gets players
    let dealtPs = addToPlayersFaceUp p ps (head cs)
    modify $ \st -> st { players = dealtPs, deck = tail cs }

-- | deal a card from the deck to the players face down hand
dealToFaceDownST :: (MonadState Game m) => Player -> m ()
dealToFaceDownST p = do
    cs <- gets deck
    ps <- gets players
    let dealtPs = addToPlayersFaceDown p ps (head cs)
    modify $ \st -> st { players = dealtPs, deck = tail cs }

-- | deal the cards to the players
dealST :: (MonadState Game m) => m [[()]]
dealST = do
    cardsEach  <- gets numCardsEach
    playerList <- gets players
    forM playerList (\p -> 
        forM [1..cardsEach] (\_ -> do
            dealToFaceDownST p
            dealToFaceUpST p 
            dealToHandST p 1))
