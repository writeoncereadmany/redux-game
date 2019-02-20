module Examples.Balls.Ball where

import Graphics.Gloss (Vector, yellow)
import ReduxGame.Entities.Entity
import ReduxGame.Components.Components
import ReduxGame.Shape.Shape

ball :: Vector -> Vector -> Entity
ball pos vel = entity
           <-+ Position pos
           <-+ Velocity vel
           <-+ circle 0 50
           <-+ yellow
           <-+ Moving
