module UI where

import Brick
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Data.Vector (fromList)
import Data.Bool (bool)
    
import Cards
import Shared


drawUI :: Game -> [Widget Name]
drawUI Game{..} = return $
    border $ leftWidget <+> vBorder <+> rightWidget 
    where
        main = vLimit 4 $ optionWidget _optionsList

        historyWidget = viewport "History" Vertical (vBox $ map (visible . str) _history)

        leftWidget  = vBox $ [historyWidget, hBorder, main]
        rightWidget = playerWidget <=> cardRules

        playerWidget = case _players of
            [] -> emptyWidget
            _  -> vBox
                [ padBottom (Pad 1) 
                        $ hCenter 
                        $ str "Players"
                , padLeft (Pad 4) 
                        $ vBox 
                        $ map (str . show) (take (length _players) _turns)
                , padTop (Pad 1) 
                        $ hBorder
                , padAll 1 $ padLeft (Pad 3) $ str $ "Cards remaining in deck: " ++ show (length _deck)
                , hBorder
                ]
        


cardRules :: Widget Name
cardRules = let
        cardWidgets = cardWidget <$> [minBound..maxBound] 
        in 
            padBottom (Pad 1) (hCenter $ str "Card Rules")
            <=> padBottom Max (vBox cardWidgets)


cardWidget :: Card -> Widget Name
cardWidget card = padBottom (Pad 1) $
    str ("(" ++ show (value card) ++ ") " ++ show card) 
        <+> padLeft Max (str $ show (numInDeck card) ++ " total")
    <=>
    padTop (Pad 1) (padLeft (Pad 4) (strWrap $ rulesText card))


optionWidget :: List Name String -> Widget Name
optionWidget optionsList = renderList draw True optionsList
  where
    draw selected label = str $ (bool "   " "-> " selected) ++ label
