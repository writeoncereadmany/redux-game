module ReduxGame.WorldShapeRenderer where

import Graphics.Gloss
import Codec.BMP

import ReduxGame.Renderer.Renderable
import ReduxGame.Renderer.ShapeRenderer
import ReduxGame.Shape
import ReduxGame.Components
import ReduxGame.Entities

instance Renderable World where
  render world = Pictures (mapStore renderShapes world ++ mapStore renderSprites world) where
    renderShapes :: (Shape, Color, Not Picture, Maybe Position) -> Picture
    renderShapes (s, c, _, (Just (Position (x, y)))) = translate x y $ color c $ render s
    renderShapes (s, c, _, Nothing) = color c $ render s
    renderSprites :: (Picture, Position) -> Picture
    renderSprites (p, Position (x, y)) = translate x y $ p
