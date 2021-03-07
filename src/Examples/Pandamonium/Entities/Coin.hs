module Examples.Pandamonium.Entities.Coin where

import Control.Lens
import Graphics.Gloss hiding (circle)

import ReduxGame.Entities
import ReduxGame.Shape
import ReduxGame.Components

import Examples.Pandamonium.Assets.PandaAssets
import Examples.Pandamonium.Labels

newtype AnimationFrames = AnimationFrames [ Picture ] deriving Component

frames :: [ BitmapData ] -> [ Picture ]
frames images = cycle (Bitmap <$> images)

coin :: PandaAssets -> Vector -> Entity
coin assets position = entity
            <-+ Coin
            <-+ circle (0, 0) 8
            <-+ Position position
            <-+ head (frames $ assets ^. coin_sprites)
            <-+ AnimationFrames (tail (frames $ assets ^. coin_sprites))
