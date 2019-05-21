module Examples.Pandamonium.Entities.Hero where

import Control.Lens

import Graphics.Gloss hiding (circle)
import Graphics.Gloss.Interface.IO.Game

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Shape.Shape hiding (move)
import ReduxGame.Components
import ReduxGame.Collisions
import ReduxGame.Controls.Axis hiding (updateAxis)
import ReduxGame.Controls.Button

import Examples.Pandamonium.Entities.Coin

w = 64
h = 64

h_vel = 800
gravity = -2400

data Hero = Hero deriving Component

data Horizontal = Horizontal
data Jump = Jump
data JumpEvent = JumpEvent deriving ReduxEvent
data GroundedState = Grounded | Airborne deriving Component

hero :: Vector -> Entity
hero position = entity
            <-+ Hero
            <-+ rectangle (-w/2, -h/2) (w, h)
            <-+ Position position
            <-+ Velocity (0, 0)
            <-+ Acceleration (0, gravity)
            <-+ green
            <-+ Moving 0 1
            <-+ AxisType Horizontal (axis (button 'z') (button 'x'))
            <-+ ButtonType Jump ((button '.') { onPress = fireEvent JumpEvent })
            <-+ Airborne

updateAxis :: forall a . a -> Event -> Only (AxisType a) -> Events (Only (AxisType a))
updateAxis _ event (Only (AxisType a axis)) = Only . AxisType a <$> axisPress event axis

updateButton :: forall a . a -> Event -> Only (ButtonType a) -> Events (Only (ButtonType a))
updateButton _ event (Only (ButtonType a button)) = Only . ButtonType a <$> keyPress event button

move :: TimeStep -> (AxisType Horizontal, Velocity) -> Only Velocity
move (TimeStep dt) (AxisType _ axis, Velocity (_, dy))
  | axis ^. onAxis == Min = Only $ Velocity (-h_vel, dy)
  | axis ^. onAxis == Neutral = Only $ Velocity (0, dy)
  | axis ^. onAxis == Max = Only $ Velocity (h_vel, dy)

jump :: JumpEvent -> (GroundedState, Velocity) -> Only Velocity
jump _ (Grounded, Velocity (x, _)) = Only $ Velocity (x, 2000)
jump _ (_, v) = Only v

resetGroundedState :: BeforeTimeStep -> (GroundedState, Color) -> (GroundedState, Color)
resetGroundedState _ _ = (Airborne, cyan)

updateGroundedState :: Pushed -> Tagged (GroundedState, Color) -> (GroundedState, Color)
updateGroundedState (Pushed pushedEntId (x, y)) (Tagged entId (oldState, oldColor)) =
  if pushedEntId == entId && y > 0
    then (Grounded, green)
    else (oldState, oldColor)

collectCoins :: TimeStep -> World -> Events World
collectCoins = fireOnCollision Hero Coin destroyCoin where
  destroyCoin hero_id coin_id = destroy coin_id

heroRedux :: Redux World
heroRedux = redux
         |$> resetGroundedState
         |$> updateGroundedState
         |*> updateAxis Horizontal
         |*> updateButton Jump
         |$> jump
         |$> move
         |=> collectCoins
