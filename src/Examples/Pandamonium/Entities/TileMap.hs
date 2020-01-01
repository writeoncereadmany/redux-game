module Examples.Pandamonium.Entities.TileMap where

import Prelude hiding (length)
import Data.Vector

import Graphics.Gloss (Picture (Blank, Pictures), translate)

import ReduxGame.Entities
import ReduxGame.Renderer.Renderable

data Tile = Tile Picture

emptyTile = Tile Blank

instance Renderable Tile where
  render (Tile p) = p

drawTileMap :: Int -> Int -> Vector (Vector Tile) -> Picture
drawTileMap width height tiles =
  Pictures [ translate (fromIntegral (x * width)) (fromIntegral (y * height)) (render (tiles ! y ! x))
           | y <- [0 .. length tiles]
           , x <- [0 .. length (tiles ! y)]]

-- tileMap :: Int -> Int -> Vector (Vector Tile) -> Entity
-- tileMap = entity
--       <-+
