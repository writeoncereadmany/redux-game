module Examples.Pandamonium.Controllers.HeroMovement where

import Control.Lens
import Graphics.Gloss.Interface.IO.Game

import ReduxGame.Redux
import ReduxGame.Collisions
import ReduxGame.Controls.Axis
import ReduxGame.Controls.Button
import ReduxGame.Components
import ReduxGame.Entities

import Examples.Pandamonium.Labels
import Examples.Pandamonium.Events
import Examples.Pandamonium.Entities.Hero

initial_jump = 270
velocity_cap = 100
accel = 500
mu = 2

move :: TimeStep -> (AxisType Horizontal, Acceleration) -> Acceleration
move (TimeStep dt) (AxisType _ axis, a)
  | axis ^. onAxis == Min = a & x -~ accel
  | axis ^. onAxis == Neutral = a
  | axis ^. onAxis == Max = a & x +~ accel

capHorizontalSpeed :: TimeStep -> Velocity -> Velocity
capHorizontalSpeed _ v
  | v ^. x < (-velocity_cap) = v & x .~ (-velocity_cap)
  | v ^. x > velocity_cap = v & x .~ velocity_cap
  | otherwise = v

friction :: TimeStep -> (Velocity, Acceleration) -> Acceleration
friction _ (v, a) = a & x -~ (v ^. x * mu)

jump :: JumpEvent -> (GroundedState, Velocity) -> (GroundedState, Velocity)
jump _ (Grounded, v) = (Falling, v & y .~ initial_jump)
jump _ (s, v) = (s, v)


resetGroundedState :: BeforeTimeStep -> GroundedState -> GroundedState
resetGroundedState _ _ = Falling

updateGroundedState :: Pushed -> Tagged (GroundedState) -> GroundedState
updateGroundedState (Pushed pushedEntId (x, y)) (Tagged entId oldState) =
  if pushedEntId == entId && y > 0
    then Grounded
    else oldState

heroMovementRedux :: Redux World
heroMovementRedux = redux
                |$> resetGroundedState
                |$> updateGroundedState
                |$> jump
                |$> move
                |$> friction
