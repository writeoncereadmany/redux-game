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

move :: TimeStep -> (AxisType Horizontal, Velocity, Acceleration) -> Acceleration
move (TimeStep dt) (AxisType _ axis, Velocity (_, dy), Acceleration (ddx, ddy))
  | axis ^. onAxis == Min = Acceleration (ddx - accel, ddy)
  | axis ^. onAxis == Neutral = Acceleration (ddx, dy)
  | axis ^. onAxis == Max = Acceleration (ddx + accel, ddy)

capHorizontalSpeed :: TimeStep -> Velocity -> Velocity
capHorizontalSpeed _ (Velocity (dx, dy))
  | dx < (-velocity_cap) = Velocity (-velocity_cap, dy)
  | dx > velocity_cap = Velocity (velocity_cap, dy)
  | otherwise = Velocity (dx, dy)

friction :: TimeStep -> (Velocity, Acceleration) -> Acceleration
friction _ (Velocity (dx, _), Acceleration (ddx, ddy)) = Acceleration (ddx - (dx * mu), ddy)

jump :: JumpEvent -> (GroundedState, Velocity) -> (GroundedState, Velocity, Fuel)
jump _ (Grounded, Velocity (x, _)) = (Ascending, Velocity (x, initial_jump), Fuel uplift_fuel)
jump _ (s, v) = (s, v, Fuel 0)

reach :: TimeStep -> (GroundedState, Fuel, Acceleration) -> (Fuel, Acceleration)
reach (TimeStep t) (Ascending, Fuel fuel, (Acceleration (ddx, ddy))) = (Fuel (fuel - t), Acceleration (ddx, ddy + uplift))
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
