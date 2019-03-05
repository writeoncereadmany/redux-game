module Examples.Balls.Ball where

import Graphics.Gloss (Vector, yellow)
import ReduxGame.Entities.Entity
import ReduxGame.Components.Components
import ReduxGame.Collisions.CollisionEvents
import ReduxGame.Shape.Shape

ball :: Vector -> Vector -> Vector -> Entity
ball pos vel acc = entity
               <-+ Position pos
               <-+ Velocity vel
               <-+ Acceleration acc
               <-+ circle 0 20
               <-+ yellow
               <-+ Moving 1 0.95
