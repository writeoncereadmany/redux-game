module Examples.Pong.Entities.Particle where

import Graphics.Gloss (white)

import ReduxGame.Components
import ReduxGame.Shape
import ReduxGame.Entities

width = 20
height = 20
gravity = (-600)

data Particle = Particle deriving Component

data Slowdown = Slowdown Float deriving Component

particle :: Float -> Float -> Float -> Float -> Entity
particle x y dx dy = entity
                 <-+ Particle

                 <-+ Position (x, y)
                 <-+ Velocity (dx, dy)
                 <-+ Acceleration (0, 0)
                 <-+ Slowdown 0.05

                 <-+ rectangle (-width/2, -height/2) (width, height)
                 <-+ white
