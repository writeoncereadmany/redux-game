module Examples.Balls.Paddle where

import Graphics.Gloss (Vector, white)
import Graphics.Gloss.Interface.IO.Game (Event)
import ReduxGame.Redux
import ReduxGame.Shape.Shape
import ReduxGame.Entities.Entities
import ReduxGame.Entities.Entity
import ReduxGame.Entities.EntityRedux
import ReduxGame.Entities.Store.Variadics
import ReduxGame.InputEvents
import ReduxGame.Components.Components
import ReduxGame.Collisions.CollisionEvents

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

listenToMove :: Event -> Only Controlled -> Only Controlled
listenToMove event controlled@(Only (Controlled left right ldown rdown))
  | isKeyPress left event    = Only $ Controlled left right True  rdown
  | isKeyPress right event   = Only $ Controlled left right ldown True
  | isKeyRelease left event  = Only $ Controlled left right False rdown
  | isKeyRelease right event = Only $ Controlled left right ldown False
  | otherwise = controlled

movePaddle :: TimeStep -> (Controlled, Speed) -> Only Velocity
movePaddle (TimeStep t) (Controlled _ _ left right, Speed v)
  | left && right = Only $ Velocity (0, 0)
  | left = Only $ Velocity (-v, 0)
  | right = Only $ Velocity (v, 0)
  | otherwise = Only $ Velocity (0, 0)

paddleRedux :: Redux (ComponentStore MapStore)
paddleRedux = redux
          |$> listenToMove
          |$> movePaddle
