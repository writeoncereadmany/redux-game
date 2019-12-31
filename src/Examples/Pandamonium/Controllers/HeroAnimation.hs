module Examples.Pandamonium.Controllers.HeroAnimation where

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

xmod :: Facing -> Float
xmod FacingRight = 1
xmod FacingLeft = (-1)

face :: Facing -> Picture -> Picture
face f = scale (xmod f) 1

updateFacing :: AfterTimeStep -> (Velocity, GroundedState, Facing) -> Facing
updateFacing _ (Velocity (dx, dy), grounded, facing)
  | grounded == Grounded && dx < 0 = FacingLeft
  | grounded == Grounded && dx > 0 = FacingRight
  | otherwise = facing

runFrame :: Float -> Facing -> [ Picture ] -> Picture
runFrame xPos facing pictures = face facing $ pictures !! (mod (floor $ (xmod facing * xPos) / 80)) (length pictures)

float_range = 400

animate :: AfterTimeStep -> (PandaFrames, GroundedState, Facing, Position, Velocity) -> Picture
animate _ (frames, grounded, facing, Position (x, y), Velocity (dx, dy))
  | grounded == Grounded && abs dx < 50 = face facing $ frames ^. standing
  | grounded == Grounded = runFrame x facing (frames ^. run_animation)
  | dy > float_range = face facing $ frames ^. jumping
  | dy > (-float_range) = face facing $ frames ^. floating
  | otherwise = face facing $ frames ^. falling

heroAnimationRedux :: Redux World
heroAnimationRedux = redux
                 |$> updateFacing
                 |$> animate
