module Shared where
    
import Brick.Widgets.List
import Data.Map.Strict
import Lens.Micro.TH (makeLenses)
import System.Random

import Data.Set
    
type CustomEvent = ()
type Name = String

type Deck = [Card]

data Game = Game 
    { _deck        :: Deck
    , _optionsList :: List Name String
    , _state       :: GameState
    , _players     :: [Player]
    , _holding     :: Map Player Card
    , _turns       :: [Player]
    , _history     :: [String]
    , _rand        :: StdGen
    , _immune      :: Set Player
    , _setAside    :: Card
    }

data GameState
    = Initial
    | PlayerSelect
    | InitialDraw
    | Turn Player
    | PickCard Player Card
    | Lose Player

    | GuardChoosePlayer Player
    | GuardChooseCard Player
    | GuardOutcome Player Player

    | PriestChoosePlayer Player
    | PriestOutcome Player

    | BaronChoosePlayer Player
    | BaronOutcome Player

    | BecomeImmune Player

    | PrinceChoosePlayer Player
    | PrinceOutcome Player

    | KingChoosePlayer Player
    | KingOutcome Player
    
    deriving (Show, Eq, Ord)

data Card 
    = Guard
    | Priest
    | Baron
    | Handmaiden
    | Prince
    | King
    | Countess
    | Princess
    deriving (Enum, Bounded, Show, Eq, Ord)

data Player
    = You
    | Alice
    | Bob
    | Charlie
    deriving (Enum, Bounded, Show, Eq, Ord)

makeLenses ''Game