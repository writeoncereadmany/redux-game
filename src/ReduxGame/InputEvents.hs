module ReduxGame.InputEvents where

import Graphics.Gloss.Interface.IO.Game

isKeyPress :: Char -> Event -> Bool
isKeyPress expected (EventKey (Char actual) Down _ _) = expected == actual
isKeyPress _ _ = False

isKeyRelease :: Char -> Event -> Bool
isKeyRelease expected (EventKey (Char actual) Up _ _) = expected == actual
isKeyRelease _ _ = False
