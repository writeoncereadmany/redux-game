module Examples.Pandamonium.Assets.PandaAssets where

import Control.Lens
import Graphics.Gloss

import ReduxGame.Sprites.SpriteSheet

data PandaAssets = PandaAssets
  { _coin_sprites :: BitmapData

  }

makeLenses ''PandaAssets

loadAssets :: IO PandaAssets
loadAssets = do
  coins <- loadBitmapData "resources/Examples/Pandamonium/Sprites/coin.bmp"
  return $ PandaAssets coins
