module Examples.Pandamonium.Entities.Wall where

import Graphics.Gloss

import ReduxGame.Entities
import ReduxGame.Shape.Shape
import ReduxGame.Components
import ReduxGame.Collisions

wall :: Vector -> Vector -> Entity
wall bottomLeft dimensions = entity
                         <-+ rectangle bottomLeft dimensions
                         <-+ white
                         <-+ Static 1
