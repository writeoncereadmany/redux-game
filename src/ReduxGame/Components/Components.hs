module ReduxGame.Components.Components where

import Graphics.Gloss (Vector, Color, Picture)

import ReduxGame.Redux
import ReduxGame.Entities.Component
import ReduxGame.Entities.Entity
import ReduxGame.Shape.Shape

class NamedBox a b where
  unwrap :: a -> b

data Position = Position Vector deriving Component
instance NamedBox Position Vector where unwrap (Position p) = p

data Velocity = Velocity Vector deriving Component
instance NamedBox Velocity Vector where unwrap (Velocity v) = v

data Acceleration = Acceleration Vector deriving Component
instance NamedBox Acceleration Vector where unwrap (Acceleration a) = a

instance Component Shape
instance Component Color
instance Component Picture

applyAcceleration :: TimeStep -> (Acceleration, Velocity) -> Velocity
applyAcceleration (TimeStep t) (Acceleration (ddx, ddy), Velocity (dx, dy)) =
  Velocity (dx + ddx * t, dy + ddy * t)

applyVelocity :: TimeStep -> (Velocity, Position) -> Position
applyVelocity (TimeStep t) (Velocity (dx, dy), Position (x, y)) =
  Position (x + dx * t, y + dy * t)
