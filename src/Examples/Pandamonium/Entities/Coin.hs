module Examples.Pandamonium.Entities.Coin where

import Graphics.Gloss hiding (circle)

import ReduxGame.Entities
import ReduxGame.Shape
import ReduxGame.Components
import ReduxGame.Collisions

import Examples.Pandamonium.Labels

coin :: Vector -> Entity
coin position = entity
            <-+ Coin
            <-+ circle 0 30
            <-+ Position position
            <-+ yellow
