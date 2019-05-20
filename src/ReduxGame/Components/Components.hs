module ReduxGame.Components.Components where

import Graphics.Gloss (Vector, Color)

import ReduxGame.Redux
import ReduxGame.Entities.Entity
import ReduxGame.Entities.Store.Variadics
import ReduxGame.Shape.Shape

data Position = Position Vector deriving Component
instance Default Position where defaultValue = Position (0,0)

data Velocity = Velocity Vector deriving Component
instance Default Velocity where defaultValue = Velocity (0,0)

data Acceleration = Acceleration Vector deriving Component
instance Default Acceleration where defaultValue = Acceleration (0,0)

instance Component Shape
instance Component Color

integrate :: TimeStep -> (Position, Velocity, Acceleration) -> (Position, Velocity)
integrate (TimeStep t) ((Position (x, y)), (Velocity (dx, dy)), (Acceleration (ddx, ddy))) =
  let (dx', dy') = (dx + ddx * t, dy + ddy * t)
      (x', y') = (x + dx' * t, y + dy' * t)
   in (Position (x', y'), Velocity (dx', dy'))
