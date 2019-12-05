module Examples.Pandamonium.Controllers.HeroMovement where

import Control.Lens
import Graphics.Gloss.Interface.IO.Game

import ReduxGame.Redux
import ReduxGame.Collisions
import ReduxGame.Controls.Axis
import ReduxGame.Components
import ReduxGame.Entities

import Examples.Pandamonium.Labels
import Examples.Pandamonium.Events
import Examples.Pandamonium.Entities.Hero

h_vel = 800

move :: TimeStep -> (AxisType Horizontal, Velocity) -> Velocity
move (TimeStep dt) (AxisType _ axis, Velocity (_, dy))
  | axis ^. onAxis == Min = Velocity (-h_vel, dy)
  | axis ^. onAxis == Neutral = Velocity (0, dy)
  | axis ^. onAxis == Max = Velocity (h_vel, dy)

jump :: JumpEvent -> (GroundedState, Velocity) -> Velocity
jump _ (Grounded, Velocity (x, _)) = Velocity (x, 2000)
jump _ (_, v) = v

resetGroundedState :: BeforeTimeStep -> GroundedState -> GroundedState
resetGroundedState _ _ = Airborne

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
