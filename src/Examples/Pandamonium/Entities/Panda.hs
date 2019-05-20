module Examples.Pandamonium.Entities.Panda where

import Control.Lens

import Graphics.Gloss hiding (circle)
import Graphics.Gloss.Interface.IO.Game

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Shape.Shape hiding (move)
import ReduxGame.Components.Components
import ReduxGame.Collisions.CollisionEvents
import ReduxGame.Controls.Axis hiding (updateAxis)
import ReduxGame.Controls.Button

w = 64
h = 64

dx = 400

data Horizontal = Horizontal

panda :: Vector -> Entity
panda position = entity
             <-+ rectangle (position - (w/2, h/2)) (w, h)
             <-+ Position position
             <-+ green
             <-+ Static 1
             <-+ AxisType Horizontal (axis (button 'z') (button 'x'))

updateAxis :: forall a . a -> Event -> Only (AxisType a) -> Events (Only (AxisType a))
updateAxis _ event (Only (AxisType a axis)) = Only <$> AxisType a <$> axisPress event axis

move :: TimeStep -> (AxisType Horizontal, Position) -> Only Position
move (TimeStep dt) (AxisType _ axis, Position (x, y))
  | axis ^. onAxis == Min = Only $ Position (x -dx*dt, y)
  | axis ^. onAxis == Neutral = Only $ Position (x, y)
  | axis ^. onAxis == Max = Only $ Position (x + dx*dt, y)

pandaRedux :: Redux World
pandaRedux = redux
         |*> updateAxis Horizontal
         |$> move
