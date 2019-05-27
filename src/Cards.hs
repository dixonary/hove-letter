module Cards where

import Shared


defaultDeck :: [Card]
defaultDeck = concatMap (\c -> replicate (numInDeck c) c) allCards

allCards :: [Card]
allCards = [minBound..maxBound]

value :: Card -> Int
value = (+1) . fromEnum


rulesText :: Card -> String
rulesText Guard = "Pick an opponent and name a (non-guard) card. If they are holding a card of that type, they lose."
rulesText Priest = "Pick an opponent. Look at their card."
rulesText Baron = "Compare your held card with an opponent's held card. The player with the lower valued card loses."
rulesText Handmaiden = "Effects which target you do not work until your next turn."
rulesText Prince = "Choose a player. They must discard their card and immediately draw a new one. If they discarded the Princess, they lose."
rulesText King = "Choose a player. Switch held cards with them."
rulesText Countess = "If held at the same time as King or Prince, this card MUST be discarded."
rulesText Princess = "If a player ever discards this card, that player loses."


numInDeck :: Card -> Int
numInDeck Guard      = 5
numInDeck Priest     = 2
numInDeck Baron      = 2
numInDeck Handmaiden = 2
numInDeck Prince     = 2
numInDeck King       = 1
numInDeck Countess   = 1
numInDeck Princess   = 1