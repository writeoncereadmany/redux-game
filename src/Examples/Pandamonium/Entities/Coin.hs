module Examples.Pandamonium.Entities.Coin where

import Graphics.Gloss hiding (circle)

import ReduxGame.Entities
import ReduxGame.Shape.Shape
import ReduxGame.Components.Components
import ReduxGame.Collisions.CollisionEvents

coin :: Vector -> Entity
coin position = entity
            <-+ circle 0 30
            <-+ Position position
            <-+ yellow
            <-+ Static 1
