module ReduxGame.Shape.Shape where

import Graphics.Gloss (Vector)

data Shape = Circle Vector Float
           | Polygon [ Vector ] [ Vector ]
           deriving (Show, Eq)
