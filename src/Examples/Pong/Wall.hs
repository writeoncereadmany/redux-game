module Examples.Pong.Wall where

import Graphics.Gloss (white)

import ReduxGame.Entities
import ReduxGame.Shape
import ReduxGame.Components
import ReduxGame.Collisions

wall :: Float -> Float -> Float -> Float -> Entity
wall x y w h = entity
           <-+ rectangle (x, y) (w, h)
           <-+ white
           <-+ Static 1
