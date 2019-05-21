module Examples.Pandamonium.Entities.Hero where

import Control.Lens

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Shape
import ReduxGame.Components
import ReduxGame.Collisions
import ReduxGame.Controls

import Examples.Pandamonium.Labels
import Examples.Pandamonium.Events

w = 64
h = 64

gravity = -2400

data GroundedState = Grounded | Airborne deriving Component

hero :: Vector -> Entity
hero position = entity
            <-+ Hero

            <-+ rectangle (-w/2, -h/2) (w, h)
            <-+ green

            <-+ Position position
            <-+ Velocity (0, 0)
            <-+ Acceleration (0, gravity)

            <-+ AxisType Horizontal (axis (button 'z') (button 'x'))
            <-+ ButtonType Jump ((button '.') { onPress = fireEvent JumpEvent })

            <-+ Moving 0 1
            <-+ FeelsGravity
            <-+ Airborne
