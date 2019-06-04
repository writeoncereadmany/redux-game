module ReduxGame.Sprites.SpriteSheet where

import Control.Lens
import Graphics.Gloss
import Codec.BMP
import qualified Data.ByteString as B
import Data.Word
import Debug.Trace

data Bounds = Bounds
  { _left :: Int
  , _top :: Int
  , _width :: Int
  , _height :: Int
  }

makeLenses ''Bounds

loadBMP :: String -> IO BMP
loadBMP filename = do picture <- readBMP filename
                      case picture of
                        (Right sprite) -> return sprite
                        (Left e)       -> error $ show e

extract_sprite :: BMP -> Bounds -> BMP
extract_sprite sheet sprite_bounds = let
     sheet_bytes = unpackBMPToRGBA32 sheet
     slices = extract_slices (bounds_of sheet) sprite_bounds
     sprite_bytes = foldl B.append B.empty (slice sheet_bytes <$> slices)
  in packRGBA32ToBMP (sprite_bounds ^. width) (sprite_bounds ^. height) sprite_bytes

bounds_of :: BMP -> Bounds
bounds_of (bmpDimensions -> (x, y)) = Bounds { _left = 0, _top = 0, _width = x, _height = y}

slice :: B.ByteString -> (Int, Int) -> B.ByteString
slice bytes (start, end) = B.take (end - start + 1) $ B.drop start $ bytes

extract_slices :: Bounds -> Bounds -> [(Int, Int)]
extract_slices image sub_image = let
     first_slice_start = sub_image ^. left + (sub_image ^. top * image ^. width)
     first_slice = (first_slice_start, first_slice_start + sub_image ^. width - 1)
  in build_slices first_slice <$> [1..(sub_image ^. height)]
  where build_slices (start, end) n = let offset = (image ^. width) * (n-1) in (start + offset, end + offset)
