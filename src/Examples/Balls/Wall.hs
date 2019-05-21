module Examples.Balls.Wall where

import Graphics.Gloss (Vector, white)
import ReduxGame.Shape.Shape
import ReduxGame.Entities
import ReduxGame.Components
import ReduxGame.Collisions

wall :: Vector -> Vector -> Entity
wall bottomLeft dimensions = entity
                         <-+ rectangle bottomLeft dimensions
                         <-+ white
                         <-+ Static 0.95
