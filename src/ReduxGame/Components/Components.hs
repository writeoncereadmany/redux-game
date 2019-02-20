module ReduxGame.Components.Components where

import Graphics.Gloss (Vector, Color)

import ReduxGame.Entities.Entity
import ReduxGame.Shape.Shape

data Position = Position Vector deriving Component
data Velocity = Velocity Vector deriving Component
data Acceleration = Acceleration Vector deriving Component

data Static = Static deriving Component
data Moving = Moving deriving Component

instance Component Shape
instance Component Color
