module ReduxGame.WorldShapeRenderer where

import Graphics.Gloss
import Codec.BMP

import ReduxGame.Renderer.Renderable
import ReduxGame.Renderer.ShapeRenderer
import ReduxGame.Shape
import ReduxGame.Components
import ReduxGame.Entities

data Camera = Camera deriving Component

instance Renderable World where
  render world =
    let (x, y) = cameraPosition (firstComponent world)
     in translate x y $ Pictures (mapStore renderShapes world ++ mapStore renderSprites world)
        where
          cameraPosition :: Maybe (Camera, Position) -> Vector
          cameraPosition (Just (_, Position (x, y))) = (-x, -y)
          cameraPosition Nothing = (0,0)
          renderShapes :: (Shape, Color, Not Picture, Maybe Position) -> Picture
          renderShapes (s, c, _, (Just (Position (x, y)))) = translate x y $ color c $ render s
          renderShapes (s, c, _, Nothing) = color c $ render s
          renderSprites :: (Picture, Position) -> Picture
          renderSprites (p, Position (x, y)) = translate x y $ p
