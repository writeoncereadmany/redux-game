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

loadBitmapData :: Word8 -> Word8 -> Word8 -> String -> IO BitmapData
loadBitmapData r g b filename = bitmapDataOfBMP
                      <$> convertToTransparency r g b
                      <$> either (error . show) id
                      <$> readBMP filename

convertToTransparency :: Word8 -> Word8 -> Word8 -> BMP -> BMP
convertToTransparency r g b bitmap = let
      (x, y) = bmpDimensions bitmap
      bytes = unpackBMPToRGBA32 bitmap
      withTrans = unpixels $ replaceWithTransparency r g b <$> pixels bytes
   in packRGBA32ToBMP32 x y withTrans

pixels :: B.ByteString -> [[Word8]]
pixels bitmap = pixels' [] (B.unpack bitmap) where
  pixels' :: [[Word8]] -> [Word8] -> [[Word8]]
  pixels' acc [] = acc
  pixels' acc rest = let (pixel, rest') = splitAt 4 rest
                      in pixels' (acc ++ [pixel]) rest'

replaceWithTransparency :: Word8 -> Word8 -> Word8 -> [Word8] -> [Word8]
replaceWithTransparency r g b [r',g',b',a'] = if r == r' && g == g' && b == b'
  then [0, 0, 0, 0]
  else [r', g', b', a']

unpixels :: [[Word8]] -> B.ByteString
unpixels = B.pack . join

loadAssets :: IO PandaAssets
loadAssets = do
  coins <- loadBitmapData 0 0 0 "resources/Examples/Pandamonium/Sprites/coin.bmp"
  pandas <- loadBitmapData 255 0 255 "resources/Examples/Pandamonium/Sprites/pandas.bmp"
  return $ PandaAssets coins pandas
