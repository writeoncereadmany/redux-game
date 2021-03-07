module Examples.Pandamonium.Assets.PandaAssets where

import Data.Word
import Data.List
import Data.List.Split
import Control.Monad
import qualified Data.ByteString as B
import Control.Lens
import Codec.BMP
import Graphics.Gloss

data PandaAssets = PandaAssets
  { _coin_sprites :: [ BitmapData ]
  , _panda_sprites :: [ BitmapData ]
  }

makeLenses ''PandaAssets

loadBitmapData :: (Int, Int) -> (Word8, Word8, Word8) -> String -> IO [ BitmapData ]
loadBitmapData tileSize transColour filename = do 
  (Right bmp) <- readBMP filename 
  return $ bitmapDataOfBMP <$> sprites tileSize transColour bmp


sprites :: (Int, Int) -> (Word8, Word8, Word8) -> BMP -> [ BMP ]
sprites tileSize@(w, h) transColour bitmap = let
      mapSize = bmpDimensions bitmap
      bytes = unpackBMPToRGBA32 bitmap
      pixels = chunksOf 4 $ B.unpack bytes
      withTransparency = replaceWithTransparency transColour <$> pixels
      tiles = toTiles mapSize tileSize withTransparency 
      tileBytes = B.pack <$> tiles
   in packRGBA32ToBMP32 w h <$> tileBytes

type Pixel = [Word8]
type Slice = [Pixel]
type Tile = [Pixel]

toTiles :: (Int, Int) -> (Int, Int) -> [[Word8]] -> [[Word8]]
toTiles (map_width, map_height) (tile_width, tile_height) bytes = 
  let
     numCols = divExact map_width tile_width
     numRows = divExact map_height tile_height
     slices = chunksOf tile_width bytes -- bytes = List (Vec 4 Word), slices = List (Vec tw (Vec 4 Word))
     rows = chunksOf (numCols * tile_height) slices -- rows = List (Vec numRows (Vec tw (Vec 4 Word)))
     rowTiles = tilesFromRowData numCols <$> rows
  in join <$> join rowTiles

tilesFromRowData :: Int -> [Slice] -> [Tile]
tilesFromRowData numCols slices = let 
    tileSlices = transpose $ chunksOf numCols slices
 in join <$> tileSlices

divExact :: Int -> Int -> Int 
divExact num divisor = case quotRem num divisor of 
    (x, 0) -> x
    (_, _) -> error "Does not divide evenly"

replaceWithTransparency :: (Word8, Word8, Word8) -> [Word8] -> [Word8]
replaceWithTransparency (r, g, b) [r',g',b',a'] = if r == r' && g == g' && b == b'
  then [0, 0, 0, 0]
  else [r', g', b', a']

loadAssets :: IO PandaAssets
loadAssets = do
  coins <- loadBitmapData (16, 16) (0, 0, 0) "resources/Examples/Pandamonium/Sprites/coin.bmp"
  pandas <- loadBitmapData (22, 22) (255, 0, 255) "resources/Examples/Pandamonium/Sprites/pandas.bmp"
  return $ PandaAssets coins pandas


