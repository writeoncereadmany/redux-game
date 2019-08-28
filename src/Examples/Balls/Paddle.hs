module Examples.Balls.Paddle where

import Graphics.Gloss (Vector, white)
import Graphics.Gloss.Interface.IO.Game (Event)
import ReduxGame.Redux
import ReduxGame.Shape
import ReduxGame.Entities
import ReduxGame.InputEvents
import ReduxGame.Components
import ReduxGame.Collisions

data Controlled = Controlled Char Char Bool Bool deriving Component
data Speed = Speed Float deriving Component

paddle :: Vector -> Vector -> Entity
paddle bottomLeft dimensions = entity
                           <-+ rectangle (0,0) dimensions
                           <-+ white
                           <-+ Position bottomLeft
                           <-+ Velocity (0,0)
                           <-+ Acceleration (0, 0)
                           <-+ Speed 500
                           <-+ Static 0.95
                           <-+ Controlled 'z' 'x' False False

listenToMove :: Event -> Controlled -> Controlled
listenToMove event controlled@(Controlled left right ldown rdown)
  | isKeyPress left event    = Controlled left right True  rdown
  | isKeyPress right event   = Controlled left right ldown True
  | isKeyRelease left event  = Controlled left right False rdown
  | isKeyRelease right event = Controlled left right ldown False
  | otherwise = controlled

movePaddle :: TimeStep -> (Controlled, Speed) -> Velocity
movePaddle (TimeStep t) (Controlled _ _ left right, Speed v)
  | left && right = Velocity (0, 0)
  | left = Velocity (-v, 0)
  | right = Velocity (v, 0)
  | otherwise = Velocity (0, 0)

paddleRedux :: Redux World
paddleRedux = redux
          |$> listenToMove
          |$> movePaddle
