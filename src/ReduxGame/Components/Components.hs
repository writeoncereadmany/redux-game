module ReduxGame.Components.Components where

import Control.Lens
import Graphics.Gloss (Vector, Color, Picture)

import ReduxGame.Redux
import ReduxGame.Entities.Component
import ReduxGame.Shape.Shape

class NamedBox a b where
  unwrap :: a -> b

class TwoD a where
  x :: Lens' a Float
  y :: Lens' a Float

instance TwoD Vector where x = _1; y = _2

newtype Position = Position { _pos :: Vector } deriving Component
makeLenses ''Position
instance TwoD Position where x = pos . x; y = pos . y
instance NamedBox Position Vector where unwrap = view pos

newtype Velocity = Velocity { _vel :: Vector } deriving Component
makeLenses ''Velocity
instance TwoD Velocity where x = vel . x; y = vel . y
instance NamedBox Velocity Vector where unwrap = view vel

newtype Acceleration = Acceleration { _acc :: Vector } deriving Component
makeLenses ''Acceleration
instance TwoD Acceleration where x = acc . x; y = acc . y
instance NamedBox Acceleration Vector where unwrap = view acc

instance Component Shape
instance Component Color
instance Component Picture

applyAcceleration :: TimeStep -> (Acceleration, Velocity) -> Velocity
applyAcceleration (TimeStep t) (a, v) = x +~ (a ^. x * t) $ y +~ (a ^. y * t) $ v

applyVelocity :: TimeStep -> (Velocity, Position) -> Position
applyVelocity (TimeStep t) (v, p) = x +~ (v ^. x * t) $ y +~ (v ^. y * t) $ p
