module Examples.Pong.Goal where

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Shape
import ReduxGame.Components

data Goal = Goal deriving Component

data GoalScored = GoalScored EntityId EntityId deriving ReduxEvent

goal :: Float -> Float -> Float -> Float -> Entity
goal x y w h = entity
           <-+ Goal
           <-+ rectangle (x, y) (w, h)
           <-+ Position (0, 0)
