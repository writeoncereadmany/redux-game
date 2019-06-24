module Examples.Pandamonium.Entities.Coin where

import Control.Lens
import Graphics.Gloss hiding (circle)

import ReduxGame.Entities
import ReduxGame.Shape
import ReduxGame.Components
import ReduxGame.Collisions

import Examples.Pandamonium.Assets.PandaAssets
import Examples.Pandamonium.Labels

data AnimationFrames = AnimationFrames [ Picture ] deriving Component

sprites = [Rectangle (n*16, 0) (16, 16) | n <- [0..5]]

frames :: BitmapData -> [ Picture ]
frames image = cycle [ scale 4 4 (BitmapSection r image) | r <- sprites ]

coin :: PandaAssets -> Vector -> Entity
coin assets position = entity
            <-+ Coin
            <-+ circle (0, 0) 32
            <-+ Position position
            <-+ head (frames $ assets ^. coin_sprites)
            <-+ AnimationFrames (tail (frames $ assets ^. coin_sprites))
