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

updateFacing :: AfterTimeStep -> (Velocity, GroundedState, Facing) -> Facing
updateFacing _ (Velocity (dx, dy), grounded, facing)
  | grounded == Grounded && dx < 0 = FacingLeft
  | grounded == Grounded && dx > 0 = FacingRight
  | otherwise = facing

xmod :: Facing -> Float
xmod FacingRight = 1
xmod FacingLeft = (-1)

face :: Facing -> Picture -> Picture
face f = scale (xmod f) 1

runFrame :: Float -> Facing -> [ Picture ] -> Picture
runFrame xPos facing pictures = face facing $ pictures !! (mod (floor $ (xmod facing * xPos) / 50)) (length pictures)

animate :: AfterTimeStep -> (PandaFrames, GroundedState, Facing, Position, Velocity) -> Picture
animate _ (frames, grounded, facing, Position (x, y), Velocity (dx, dy))
  | grounded == Grounded && abs dx < 50 = face facing $ frames ^. standing
  | grounded == Grounded = runFrame x facing (frames ^. run_animation)
  | dy > 0 = face facing $ frames ^. jumping
  | otherwise = face facing $ frames ^. falling

heroRedux :: Redux World
heroRedux = redux
         |$> resetGroundedState
         |$> updateGroundedState
         |$> jump
         |$> move
         |$> updateFacing
         |$> animate
