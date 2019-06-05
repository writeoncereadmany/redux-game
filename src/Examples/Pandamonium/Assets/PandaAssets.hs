module Examples.Pandamonium.Assets.PandaAssets where

import Control.Lens
import Codec.BMP
import Graphics.Gloss

data PandaAssets = PandaAssets
  { _coin_sprites :: BitmapData

  }

makeLenses ''PandaAssets

loadBitmapData :: String -> IO BitmapData
loadBitmapData filename = do bmp <- readBMP filename
                             case bmp of
                               (Right bitmap) -> return $ bitmapDataOfBMP bitmap
                               (Left e)       -> error $ show e

loadAssets :: IO PandaAssets
loadAssets = do
  coins <- loadBitmapData "resources/Examples/Pandamonium/Sprites/coin.bmp"
  return $ PandaAssets coins
