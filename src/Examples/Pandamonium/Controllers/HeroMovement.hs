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

updateFacing :: AfterTimeStep -> (Velocity, GroundedState, Facing) -> Only Facing
updateFacing _ (Velocity (dx, dy), grounded, facing)
  | grounded == Grounded && dx < 0 = Only FacingLeft
  | grounded == Grounded && dx > 0 = Only FacingRight
  | otherwise = Only facing

xmod :: Facing -> Float
xmod FacingRight = 1
xmod FacingLeft = (-1)

face :: Facing -> Picture -> Picture
face f = scale (xmod f) 1

runFrame :: Float -> Facing -> [ Picture ] -> Picture
runFrame xPos facing pictures = face facing $ pictures !! (mod (floor $ (xmod facing * xPos) / 50)) (length pictures)

animate :: AfterTimeStep -> (PandaFrames, GroundedState, Facing, Position, Velocity) -> Only Picture
animate _ (frames, grounded, facing, Position (x, y), Velocity (dx, dy))
  | grounded == Grounded && abs dx < 50 = Only $ face facing $ frames ^. standing
  | grounded == Grounded = Only $ runFrame x facing (frames ^. run_animation)
  | dy > 0 = Only $ face facing $ frames ^. jumping
  | otherwise = Only $ face facing $ frames ^. falling

heroRedux :: Redux World
heroRedux = redux
         |$> resetGroundedState
         |$> updateGroundedState
         |$> jump
         |$> move
         |$> updateFacing
         |$> animate
