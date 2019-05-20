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

ddx = 1200
gravity = -2400

data Horizontal = Horizontal
data Jump = Jump
data JumpEvent = JumpEvent deriving ReduxEvent

panda :: Vector -> Entity
panda position = entity
             <-+ rectangle (-w/2, -h/2) (w, h)
             <-+ Position position
             <-+ Velocity (0, 0)
             <-+ Acceleration (0, gravity)
             <-+ green
             <-+ Moving 0 1
             <-+ AxisType Horizontal (axis (button 'z') (button 'x'))
             <-+ ButtonType Jump ((button '.') { onPress = fireEvent JumpEvent })

updateAxis :: forall a . a -> Event -> Only (AxisType a) -> Events (Only (AxisType a))
updateAxis _ event (Only (AxisType a axis)) = Only . AxisType a <$> axisPress event axis

updateButton :: forall a . a -> Event -> Only (ButtonType a) -> Events (Only (ButtonType a))
updateButton _ event (Only (ButtonType a button)) = Only . ButtonType a <$> keyPress event button

move :: TimeStep -> (AxisType Horizontal, Acceleration) -> Only Acceleration
move (TimeStep dt) (AxisType _ axis, Acceleration (_, ddy))
  | axis ^. onAxis == Min = Only $ Acceleration (-ddx, ddy)
  | axis ^. onAxis == Neutral = Only $ Acceleration (0, ddy)
  | axis ^. onAxis == Max = Only $ Acceleration (ddx, ddy)

jump :: JumpEvent -> Only Velocity -> Only Velocity
jump _ (Only (Velocity (x, _))) = Only $ Velocity (x, 2000)

pandaRedux :: Redux World
pandaRedux = redux
         |*> updateAxis Horizontal
         |*> updateButton Jump
         |$> jump
         |$> move
