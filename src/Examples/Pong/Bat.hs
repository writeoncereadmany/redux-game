module Examples.Pong.Bat where

import Control.Lens
import Graphics.Gloss (white)

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Shape (rectangle)
import ReduxGame.Controls
import ReduxGame.Components
import ReduxGame.Collisions

width = 30
height = 200
moveSpeed = 1000
minPos = (-580)
maxPos = 580

data Vertical = Vertical

moveBat :: TimeStep -> (AxisType Vertical, Position) -> Position
moveBat (TimeStep t) (AxisType _ axis, Position (x, y)) = case axis ^. onAxis of
  Min -> Position (x, max (y - moveSpeed * t) minPos)
  Max -> Position (x, min (y + moveSpeed * t) maxPos)
  Neutral -> Position (x, y)

bat :: Float -> Float -> Char -> Char -> Entity
bat x y up down = entity
              <-+ Position (x, y)
              <-+ rectangle (-width/2, -height/2) (width, height)
              <-+ AxisType Vertical (axis (button down) (button up))
              <-+ white
              <-+ Static 1


batRedux :: Redux World
batRedux = worldRedux
       |$> moveBat
       |*> updateAxis Vertical
