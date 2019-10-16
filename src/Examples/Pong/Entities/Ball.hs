module Examples.Pong.Entities.Ball where

import Graphics.Gloss (white)

import ReduxGame.Entities
import ReduxGame.Shape
import ReduxGame.Components
import ReduxGame.Collisions

width = 50
height = 50

data Ball = Ball deriving Component

ball :: Float -> Float -> Float -> Float -> Entity
ball x y dx dy = entity
             <-+ Ball
             <-+ Position (x, y)
             <-+ Velocity (dx, dy)
             <-+ rectangle (-width/2, -height/2) (width, height)
             <-+ white
             <-+ Moving 1.2 1
