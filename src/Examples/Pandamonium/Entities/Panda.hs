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
             <-+ Velocity (0, 0)
             <-+ Acceleration (0, 0)
             <-+ green
             <-+ Moving 0 1
             <-+ AxisType Horizontal (axis (button 'z') (button 'x'))

updateAxis :: forall a . a -> Event -> Only (AxisType a) -> Events (Only (AxisType a))
updateAxis _ event (Only (AxisType a axis)) = Only <$> AxisType a <$> axisPress event axis

move :: TimeStep -> Only (AxisType Horizontal) -> Only Velocity
move (TimeStep dt) (Only (AxisType _ axis))
  | axis ^. onAxis == Min = Only $ Velocity (-dx, 0)
  | axis ^. onAxis == Neutral = Only $ Velocity (0, 0)
  | axis ^. onAxis == Max = Only $ Velocity (dx, 0)

pandaRedux :: Redux World
pandaRedux = redux
         |*> updateAxis Horizontal
         |$> move
