module Examples.Pandamonium.Assets.PandaAssets where

import Data.Word
import Control.Monad
import qualified Data.ByteString as B
import Control.Lens
import Codec.BMP
import Graphics.Gloss

data PandaAssets = PandaAssets
  { _coin_sprites :: BitmapData
  , _panda_sprites :: BitmapData
  }

makeLenses ''PandaAssets

loadBitmapData :: String -> IO BitmapData
loadBitmapData filename = bitmapDataOfBMP
                      <$> convertToTransparency
                      <$> either (error . show) id
                      <$> readBMP filename

convertToTransparency :: BMP -> BMP
convertToTransparency bitmap = let
      (x, y) = bmpDimensions bitmap
      bytes = unpackBMPToRGBA32 bitmap
      withTrans = unpixels $ replaceWithTransparency <$> pixels bytes
   in packRGBA32ToBMP32 x y withTrans

pixels :: B.ByteString -> [[Word8]]
pixels bitmap = pixels' [] (B.unpack bitmap) where
  pixels' :: [[Word8]] -> [Word8] -> [[Word8]]
  pixels' acc [] = acc
  pixels' acc rest = let (pixel, rest') = splitAt 4 rest
                      in pixels' (acc ++ [pixel]) rest'

replaceWithTransparency :: [Word8] -> [Word8]
replaceWithTransparency [0,0,0,_] = [0,0,0,0]
replaceWithTransparency other = other

unpixels :: [[Word8]] -> B.ByteString
unpixels = B.pack . join

loadAssets :: IO PandaAssets
loadAssets = do
  coins <- loadBitmapData "resources/Examples/Pandamonium/Sprites/coin.bmp"
  pandas <- loadBitmapData "resources/Examples/Pandamonium/Sprites/panda.bmp"
  return $ PandaAssets coins pandas
