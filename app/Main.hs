module Main where

import Brick
import Brick.Widgets.List hiding (splitAt)
    
import Graphics.Vty (defAttr, Key(..), Event(..), Modifier(..))
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), over, set)
import Lens.Micro.Extras (view)
import Control.Monad (void)
import Data.Traversable (for)
import Data.Maybe (fromJust)
import qualified Data.Vector as Vector
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import Data.Map ((!))
import Data.Tuple (swap)
import Data.List (delete)

import Data.Bifunctor (first, second)

import Prelude hiding (log)
import System.Random

import UI
import Cards
import Shared
import Shuffle




-- * App Initialisation and Event Handling

main :: IO ()
main = do
    stdGen <- getStdGen
    void $ defaultMain app (initialGame { _rand = stdGen })

app :: App Game CustomEvent Name
app = App {..}
  where
    appDraw         = drawUI
    appChooseCursor = neverShowCursor
    appHandleEvent  = handleEvent
    appAttrMap      = const $ attrMap defAttr []
    appStartEvent   = return


handleEvent :: Game -> BrickEvent Name CustomEvent -> EventM Name (Next Game)
handleEvent g@Game{..} (VtyEvent e) = do

    -- Perform any movements in the options list
    newList <- handleListEvent e _optionsList 
    let g' = set optionsList newList g

    case e of
        -- End on Ctrl+C
        EvKey (KChar 'c') [MCtrl] -> halt g'
        -- Advance on Enter
        EvKey (KEnter) _          -> continue $ advance _state g'
        -- Ignore any other keypresses
        _                         -> continue g'


-- Ignore any other kinds of events
handleEvent Game{..} _ = continue Game{..}




-- * Game Logic

initialGame :: Game
initialGame = Game
    { _deck           = defaultDeck
    , _optionsList    = list "Options" mempty 1
    , _state          = Initial
    , _holding        = mempty
    , _turns          = []
    , _players        = []
    , _history        = []
    , _rand           = error "Randomizer not yet initialised" 
    , _immune         = mempty
    , _setAside       = error "No card is set aside yet"
    }
    & log "Please select a size of game:"
    & setOptions numPlayersOpts
    & set state PlayerSelect



numPlayersOpts :: [String]
numPlayersOpts = (++ " Players") . show <$> [2..4]

advanceOnlyOpts :: [String]
advanceOnlyOpts = ["Advance"]




-- The big one! Move one step through the simulation.
advance :: GameState -> Game -> Game
advance Initial g@Game{..} = initialGame & set rand _rand

-- Jump straight to a win if all others were eliminated
advance _ g@Game{..} | length _players == 1 = 
    g 
    & log (show (head _players) ++ " wins!")
    & setOptions advanceOnlyOpts
    & set state Initial


advance PlayerSelect g@Game{..} = 
    let
        numPlayers = 2 + selectedIndex _optionsList
        newPlayers = take numPlayers [minBound..]
        newTurns   = You : cycle newPlayers
        
        (deck', rand') = shuffle _deck _rand

    in 
        g
        & set players newPlayers
        & set turns newTurns
        & log (show numPlayers ++ "-player game selected.")
        & set deck deck'
        & set rand rand'
        & set state InitialDraw
        & setOptions advanceOnlyOpts

advance InitialDraw g@Game{..} = 
    let
        ([aside], deck') = draw 1 _deck
        (drawn, deck'') = draw (length _players) deck'
    in 
        g
        & set holding (Map.fromList $ zip _players drawn)
        & set setAside aside
        & set deck deck''
        & log "One card is set aside."
        & log "All players draw a card."
        & nextTurn
        & setOptions advanceOnlyOpts


advance (Turn You) g@Game{..} =
    if _deck == []
    then
        endGame g
    else
        let 
            ([drawn], deck') = draw 1 _deck
        in
            g
            & set deck deck'
            & log " "
            & log ("You drew a " ++ show drawn ++ ".") 
            & log "Select a card to play:"
            & setOptions [show $ _holding ! You, show drawn]
            & removeImmunity You
            & set state (PickCard You drawn)

advance (Turn opponent) g@Game{..} =
    if _deck == []
    then
        endGame g
    else
        let
            ([drawn], deck') = draw 1 _deck
        in
            g
            & set deck deck'
            & log " "
            & log (show opponent ++ " drew a card.")
            & removeImmunity opponent
            & set state (PickCard opponent drawn)


advance (PickCard player drawn) g@Game{..} =
    let
        -- Order the cards in cost order, just to make the patterns easier
        hand@(cardHigher, cardLower) = (,)
            (max (_holding ! player) drawn)
            (min (_holding ! player) drawn)
        
        -- Enforce required choices, otherwise pick randomly
        (justHolding, justPlayed) = 
            case player of
                You -> case listSelected _optionsList of
                    Just 0 -> (drawn, _holding ! You)
                    Just 1 -> (_holding ! You, drawn)
                opponent -> case hand of        
                    (Princess, x)      -> (Princess, x)
                    (Countess, King)   -> (King, Countess)
                    (Countess, Prince) -> (Prince, Countess)
                    -- always play the lower card
                    _                  -> swap hand
    in
        g
        & over holding (Map.insert player justHolding)
        & log (show player ++ " played a " ++ show justPlayed ++ ".")
        & setOptions advanceOnlyOpts
        & outcome player (justHolding, justPlayed)
        

advance (Lose player) g@Game{..} =
    g 
    & over players (delete player)
    & over turns (filter (/= player))
    & log (show player ++ " is eliminated!")
    & setOptions advanceOnlyOpts
    & nextTurn


-- ** Guard

advance (GuardChoosePlayer You) g@Game{..} =
    g 
    & log ("Choose a player to guess the card of:")
    & setOptions (show <$> delete You _players)
    & set state (GuardChooseCard You)

advance (GuardChoosePlayer player) g@Game{..} = 
    g
    & setOptions advanceOnlyOpts
    & set state (GuardChooseCard player)    


advance (GuardChooseCard You) g@Game{..} =
    let
        chosenPlayer = (delete You _players) !! fromJust (listSelected _optionsList)
    in
    immuneNegate chosenPlayer g $ 
        g
        & log (show You ++ " picked " ++ show chosenPlayer ++ ".")
        & log ("Guess a card:")
        & setOptions (show <$> (tail [minBound..] :: [Card]))
        & set state (GuardOutcome You chosenPlayer)

advance (GuardChooseCard player) g@Game{..} =
    let
        (chosenPlayer, rand') = pick (delete player _players) _rand
        worked = not $ isImmune chosenPlayer g
    in
    immuneNegate chosenPlayer g $ 
        g
        & set rand rand'
        & log (show player ++ " picked " ++ show chosenPlayer ++ ".")
        & setOptions advanceOnlyOpts
        & set state (GuardOutcome player chosenPlayer)

    
advance (GuardOutcome player pickedPlayer) g@Game{..} =
    let
        (chosenCard, rand') = first toEnum $ case player of
            You -> (toEnum $ fromJust (listSelected _optionsList) + 1, _rand)
            _   -> pick [2..fromEnum (maxBound :: Card)] _rand 

        correctGuess = _holding ! pickedPlayer == chosenCard

        resultLog = if correctGuess
            then log (show pickedPlayer ++ " WAS holding a " ++ show chosenCard ++ "!")
            else log (show pickedPlayer ++ " was NOT holding a " ++ show chosenCard ++ ".")
    
        resultState = if correctGuess
            then set state (Lose pickedPlayer)
            else nextTurn
    in
    g
    & set rand rand'
    & log (show player ++ " guessed " ++ show chosenCard ++ ".")
    & resultLog
    & setOptions advanceOnlyOpts
    & resultState


-- ** Priest

advance (PriestChoosePlayer You) g@Game{..} =
    g 
    & log ("Choose a player to view the card of:")
    & setOptions (show <$> delete You _players)
    & set state (PriestOutcome You)

advance (PriestChoosePlayer player) g@Game{..} = 
    g
    & setOptions advanceOnlyOpts
    & set state (PriestOutcome player)    


advance (PriestOutcome player) g@Game{..} =
    let
        (chosenPlayer, rand') = case player of
            You -> (delete You _players !! fromJust (listSelected _optionsList), _rand)
            _   -> pick (delete player _players) _rand
        resultLog = case player of
            You -> show chosenPlayer ++ " is holding a " ++ show (_holding ! chosenPlayer) ++ "."
            _   -> show chosenPlayer ++ " shows " ++ show player ++ " their card."
    in
    immuneNegate chosenPlayer g $
        g
        & log (show player ++ " picked " ++ show chosenPlayer ++ ".")
        & set rand rand'
        & log resultLog
        & setOptions advanceOnlyOpts
        & nextTurn


-- ** Baron

advance (BaronChoosePlayer You) g@Game{..} =
    g 
    & log ("Choose a player to compare cards with:")
    & setOptions (show <$> delete You _players)
    & set state (BaronOutcome You)

advance (BaronChoosePlayer player) g@Game{..} = 
    g
    & setOptions advanceOnlyOpts
    & set state (BaronOutcome player)    


advance (BaronOutcome player) g@Game{..} =
    let
        (chosenPlayer, rand') = case player of
            You -> (delete You _players !! fromJust (listSelected _optionsList), _rand)
            _   -> pick (delete player _players) _rand

        isDraw = (_holding ! player) == (_holding ! chosenPlayer) 

        loser = if  (_holding ! player) < (_holding ! chosenPlayer) 
            then player
            else chosenPlayer

        result = if isDraw 
            then \g -> g
                & log "Both players have the same value card."
                & log "Nothing happens."
                & setOptions advanceOnlyOpts
                & nextTurn
            else \g -> g
                & log (show loser ++ " had the lower card.")
                & advance (Lose loser) 
    in
    immuneNegate chosenPlayer g $
        g
        & set rand rand'
        & log (show player ++ " picked " ++ show chosenPlayer ++ ".")
        & log (show player ++ " and " ++ show chosenPlayer ++ " compare cards.")
        & result
        

-- ** Handmaiden

advance (BecomeImmune player) g@Game{..} =
    g
    & log (show player ++ " became immune until their next turn.")
    & makeImmune player
    & setOptions advanceOnlyOpts
    & nextTurn


-- ** Prince

advance (PrinceChoosePlayer You) g@Game{..} =
    g 
    & log ("Choose a player to force discard:")
    & setOptions (show <$> _players)
    & set state (PrinceOutcome You)

advance (PrinceChoosePlayer player) g@Game{..} = 
    g
    & setOptions advanceOnlyOpts
    & set state (PrinceOutcome player)    


advance (PrinceOutcome player) g@Game{..} =
    let
        (chosenPlayer, rand') = case player of
            You -> (_players !! fromJust (listSelected _optionsList), _rand)
            _   -> pick _players _rand

        -- Interesting caveat: If the deck is empty, the player takes the card that was set aside.
        (drawn , deck') = case _deck of
            []     -> (_setAside, _deck)
            (d:ds) -> (d, ds)
    in
    immuneNegate chosenPlayer g $
        g
        & log (show player ++ " picked " ++ show chosenPlayer ++ ".")
        & case _holding ! chosenPlayer of
            Princess -> \g -> 
                g
                & log (show chosenPlayer ++ " discarded a " ++ show (_holding ! chosenPlayer) ++ ".")
                & set state (Lose chosenPlayer)
            _   -> \g -> 
                g
                & log (show chosenPlayer ++ " discarded a " ++ show (_holding ! chosenPlayer) ++ " and drew a new card.")
                & (if _deck == [] then log ("The deck was empty, so they drew the card that was set aside.") else id)
                & set deck deck'
                & over holding (Map.insert chosenPlayer drawn)
                & setOptions advanceOnlyOpts
                & nextTurn


-- ** King

advance (KingChoosePlayer You) g@Game{..} =
    g 
    & log ("Choose a player to switch hands with:")
    & setOptions (show <$> delete You _players)
    & set state (KingOutcome You)

advance (KingChoosePlayer player) g@Game{..} = 
    g
    & setOptions advanceOnlyOpts
    & set state (KingOutcome player)    


advance (KingOutcome player) g@Game{..} =
    let
        (chosenPlayer, rand') = case player of
            You -> (delete You _players !! fromJust (listSelected _optionsList), _rand)
            _   -> pick _players _rand

        chosenPlayerCard = _holding ! chosenPlayer
        playerCard       = _holding ! player
    in
    immuneNegate chosenPlayer g $
        g
        & log (show player ++ " picked " ++ show chosenPlayer ++ ".")
        & log (show player ++ " and " ++ show chosenPlayer ++ " switch cards.")
        & over holding (Map.insert chosenPlayer playerCard)
        & over holding (Map.insert player chosenPlayerCard)
        & setOptions advanceOnlyOpts
        & nextTurn


-- ** Generic Functions

nextTurn :: Game -> Game
nextTurn g@Game{..} =
    let
        turns' = tail _turns
    in
        g 
        & set turns turns'
        & set state (Turn (head turns'))


-- Validate the outcome of a played card
outcome :: Player -> (Card, Card) -> Game -> Game
outcome player (holding, played) g@Game{..} =
    let 
        moveOn st g = case player of
            You -> set state st g
            _   -> advance st g
    in
    g
    & case (holding, played) of
        (_, Guard)         -> advance (GuardChoosePlayer player)
        (_, Priest)        -> advance (PriestChoosePlayer player)
        (_, Baron)         -> advance (BaronChoosePlayer player)
        (_, Handmaiden)    -> advance (BecomeImmune player)
        (Countess, Prince) -> moveOn (Lose player)
        (_, Prince)        -> advance (PrinceChoosePlayer player) 
        (Countess, King)   -> moveOn (Lose player)
        (_, King)          -> advance (KingChoosePlayer player)
        (_, Countess)      -> nextTurn
        (_, Princess)      -> advance (Lose player)


immuneNegate :: Player -> Game -> Game -> Game
immuneNegate chosenPlayer origGame updatedGame =
    if isImmune chosenPlayer origGame 
    then
        origGame
        & log (show chosenPlayer ++ " is immune.")
        & log ("Nothing happened.")
        & setOptions advanceOnlyOpts
        & nextTurn
    else updatedGame


-- Jump straight to the count if there are no cards left
endGame :: Game -> Game
endGame g@Game{..} =
    let
        vals = Map.toList _holding
        highestVal = maximum $ fromEnum . snd <$> vals
        winners = fst <$> filter (\(player,card) -> player `elem` _players && fromEnum card == highestVal) vals
        winnerLog = case winners of
            [] -> "Nobody wins?"
            [x] -> show x ++ " wins!"
            ls -> concatMap show ls ++ " win!"

        logs :: [Game->Game]
        logs =  [(& (log $ "  " ++ show p ++ " : " ++ show (_holding ! p))) | p <- _players]
    in
    g
    & log " "
    & log ("There are no cards left to draw.")
    & flip (foldl (&)) logs
    & log winnerLog
    & setOptions advanceOnlyOpts
    & set state Initial






-- * Utility functions with meaningful names

removeImmunity :: Player -> Game -> Game
removeImmunity player = over immune $ Set.delete player

makeImmune :: Player -> Game -> Game
makeImmune player = over immune $ Set.insert player

isImmune :: Player -> Game -> Bool
isImmune player g = (player `elem` view immune g)

log :: String -> Game -> Game
log str = over history (++ [str])

-- Other things
draw :: Int -> Deck -> ([Card], Deck)
draw = splitAt

selectedIndex :: List s e -> Int
selectedIndex = fromJust . listSelected

nextPlayer :: Game -> Player
nextPlayer Game{..} = head _turns 

setOptions :: [String] -> Game -> Game
setOptions opts g@Game{..} = g 
    & over optionsList ( listReplace (Vector.fromList opts) (Just 0) )