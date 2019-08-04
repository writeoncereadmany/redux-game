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

move :: TimeStep -> (AxisType Horizontal, Velocity) -> Only Velocity
move (TimeStep dt) (AxisType _ axis, Velocity (_, dy))
  | axis ^. onAxis == Min = Only $ Velocity (-h_vel, dy)
  | axis ^. onAxis == Neutral = Only $ Velocity (0, dy)
  | axis ^. onAxis == Max = Only $ Velocity (h_vel, dy)

jump :: JumpEvent -> (GroundedState, Velocity) -> Only Velocity
jump _ (Grounded, Velocity (x, _)) = Only $ Velocity (x, 2000)
jump _ (_, v) = Only v

resetGroundedState :: BeforeTimeStep -> (Only GroundedState) -> (Only GroundedState)
resetGroundedState _ _ = (Only Airborne)

updateGroundedState :: Pushed -> Tagged (Only GroundedState) -> (Only GroundedState)
updateGroundedState (Pushed pushedEntId (x, y)) (Tagged entId (Only oldState)) =
  if pushedEntId == entId && y > 0
    then (Only Grounded)
    else (Only oldState)

heroRedux :: Redux World
heroRedux = redux
         |$> resetGroundedState
         |$> updateGroundedState
         |$> jump
         |$> move
