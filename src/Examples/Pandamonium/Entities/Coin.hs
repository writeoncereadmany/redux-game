module Examples.Pandamonium.Entities.Coin where

import Control.Lens
import Graphics.Gloss hiding (circle)

import ReduxGame.Entities
import ReduxGame.Shape
import ReduxGame.Components
import ReduxGame.Collisions

import Examples.Pandamonium.Assets.PandaAssets
import Examples.Pandamonium.Labels

data AnimationFrames = AnimationFrames [ Shape ] deriving Component

coin :: PandaAssets -> Vector -> Entity
coin assets position = entity
            <-+ Coin
            <-+ circle (0, 0) 0
            <-+ Position position
            <-+ assets ^. coin_sprites
