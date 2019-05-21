module Examples.Pandamonium.Entities.Hero where

import Control.Lens

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Shape (rectangle)
import ReduxGame.Components
import ReduxGame.Collisions
import ReduxGame.Controls.Axis hiding (updateAxis)
import ReduxGame.Controls.Button

import Examples.Pandamonium.Entities.Coin

w = 64
h = 64

gravity = -2400

data Hero = Hero deriving Component

data Horizontal = Horizontal
data Jump = Jump
data JumpEvent = JumpEvent deriving ReduxEvent
data GroundedState = Grounded | Airborne deriving Component

hero :: Vector -> Entity
hero position = entity
            <-+ Hero
            <-+ rectangle (-w/2, -h/2) (w, h)
            <-+ Position position
            <-+ Velocity (0, 0)
            <-+ Acceleration (0, gravity)
            <-+ green
            <-+ Moving 0 1
            <-+ AxisType Horizontal (axis (button 'z') (button 'x'))
            <-+ ButtonType Jump ((button '.') { onPress = fireEvent JumpEvent })
            <-+ Airborne
