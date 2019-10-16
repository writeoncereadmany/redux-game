module Examples.Pong.Entities.Bat where

import Graphics.Gloss (white)

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Shape (rectangle)
import ReduxGame.Controls
import ReduxGame.Components
import ReduxGame.Collisions

width = 30
height = 200

data Vertical = Vertical

bat :: Float -> Float -> Char -> Char -> Entity
bat x y up down = entity
              <-+ Position (x, y)
              <-+ rectangle (-width/2, -height/2) (width, height)
              <-+ AxisType Vertical (axis (button down) (button up))
              <-+ white
              <-+ Static 1
