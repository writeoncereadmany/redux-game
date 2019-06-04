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

applyAcceleration :: TimeStep -> (Acceleration, Velocity) -> Only Velocity
applyAcceleration (TimeStep t) (Acceleration (ddx, ddy), Velocity (dx, dy)) =
  Only $ Velocity (dx + ddx * t, dy + ddy * t)

applyVelocity :: TimeStep -> (Velocity, Position) -> Only Position
applyVelocity (TimeStep t) (Velocity (dx, dy), Position (x, y)) =
  Only $ Position (x + dx * t, y + dy * t)
