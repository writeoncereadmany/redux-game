module Examples.Balls.Wall where

import Graphics.Gloss (Vector, white)
import ReduxGame.Shape.Shape
import ReduxGame.Entities.Entity
import ReduxGame.Components.Components
import ReduxGame.Collisions.CollisionEvents

wall :: Vector -> Vector -> Entity
wall bottomLeft topRight = entity
                       <-+ rectangle bottomLeft topRight
                       <-+ Position (0, 0)
                       <-+ white
                       <-+ Static 0.95
