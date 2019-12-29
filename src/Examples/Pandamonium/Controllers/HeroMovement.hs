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

h_vel = 800
initial_jump = 1200
uplift = 5500
uplift_fuel = 0.5
velocity_cap = 1000
accel = 2400
mu = 2

newtype Fuel = Fuel Float deriving Component

move :: TimeStep -> (AxisType Horizontal, Acceleration) -> Acceleration
move (TimeStep dt) (AxisType _ axis, a)
  | axis ^. onAxis == Min = x -~ accel $ a
  | axis ^. onAxis == Neutral = a
  | axis ^. onAxis == Max = x +~ accel $ a

capHorizontalSpeed :: TimeStep -> Velocity -> Velocity
capHorizontalSpeed _ v
  | v ^. x < (-velocity_cap) = x .~ (-velocity_cap) $ v
  | v ^. x > velocity_cap = x .~ velocity_cap $ v
  | otherwise = v

friction :: TimeStep -> (Velocity, Acceleration) -> Acceleration
friction _ (v, a) = x -~ (v ^. x * mu) $ a

jump :: JumpEvent -> (GroundedState, Velocity) -> (GroundedState, Velocity, Fuel)
jump _ (Grounded, v) = (Ascending, y .~ initial_jump $ v, Fuel uplift_fuel)
jump _ (s, v) = (s, v, Fuel 0)

reach :: TimeStep -> (GroundedState, Fuel, Acceleration) -> (Fuel, Acceleration)
reach (TimeStep t) (Ascending, Fuel fuel, a) = (Fuel (fuel - t), y +~ uplift $ a)
reach _ (s, f, a) = (Fuel 0, a)

release :: TimeStep -> (GroundedState, ButtonType Jump, Fuel) -> GroundedState
release _ (gs, (ButtonType _ button), Fuel fuel)
  | gs == Ascending && (fuel <= 0 || not (held button)) = Falling
  | otherwise = gs

resetGroundedState :: BeforeTimeStep -> GroundedState -> GroundedState
resetGroundedState _ Ascending = Ascending
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
                |$> reach
                |$> release
                |$> move
                |$> friction
