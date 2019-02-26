module ReduxGame.Components.Components where

import Graphics.Gloss (Vector, Color)

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
