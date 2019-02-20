module ReduxGame.Components.Components where

import Graphics.Gloss (Vector, Color)

import ReduxGame.Entities.Entity
import ReduxGame.Shape.Shape

data Position = Position Vector deriving Component
data Velocity = Velocity Vector deriving Component

instance Component Shape
instance Component Color
