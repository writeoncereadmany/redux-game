module Examples.Pong.Controllers.Bats where

import Control.Lens

import ReduxGame.Components
import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Controls

import Examples.Pong.Entities.Bat

moveSpeed = 1000
minPos = (-580)
maxPos = 580

moveBat :: TimeStep -> (AxisType Vertical, Position) -> Position
moveBat (TimeStep t) (AxisType _ axis, Position (x, y)) = case axis ^. onAxis of
  Min -> Position (x, max (y - moveSpeed * t) minPos)
  Max -> Position (x, min (y + moveSpeed * t) maxPos)
  Neutral -> Position (x, y)

batRedux :: Redux World
batRedux = redux
       |$> moveBat
       |*> updateAxis Vertical
