module Examples.Pandamonium.Assets.PandaAssets where

import Control.Lens
import Codec.BMP
import Graphics.Gloss

data PandaAssets = PandaAssets
  { _coin_sprites :: BitmapData

  }

makeLenses ''PandaAssets

loadBitmapData :: String -> IO BitmapData
loadBitmapData filename = either (error . show) bitmapDataOfBMP <$> readBMP filename

loadAssets :: IO PandaAssets
loadAssets = do
  coins <- loadBitmapData "resources/Examples/Pandamonium/Sprites/coin.bmp"
  return $ PandaAssets coins
