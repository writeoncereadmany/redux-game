module Examples.Pandamonium.Entities.Coin where

import Graphics.Gloss hiding (circle)

import ReduxGame.Entities
import ReduxGame.Shape.Shape
import ReduxGame.Components
import ReduxGame.Collisions

data Coin = Coin deriving Component

coin :: Vector -> Entity
coin position = entity
            <-+ Coin
            <-+ circle 0 30
            <-+ Position position
            <-+ yellow
