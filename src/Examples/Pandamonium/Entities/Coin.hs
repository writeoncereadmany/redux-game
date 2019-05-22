module Examples.Pandamonium.Entities.Coin where

import Graphics.Gloss hiding (circle)

import ReduxGame.Entities
import ReduxGame.Shape
import ReduxGame.Components
import ReduxGame.Collisions

import Examples.Pandamonium.Labels

radii = cycle [20, 25, 30, 35, 40]

shapes :: [ Shape ]
shapes = (\r -> circle (0, 0) r) <$> radii

data AnimationFrames = AnimationFrames [ Shape ] deriving Component

coin :: Vector -> Entity
coin position = entity
            <-+ Coin
            <-+ head shapes
            <-+ (AnimationFrames $ tail shapes)
            <-+ Position position
            <-+ yellow
