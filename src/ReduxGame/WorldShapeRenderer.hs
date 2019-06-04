module ReduxGame.WorldShapeRenderer where

import Graphics.Gloss
import Codec.BMP

import ReduxGame.Renderer.Renderable
import ReduxGame.Renderer.ShapeRenderer
import ReduxGame.Shape
import ReduxGame.Components
import ReduxGame.Entities

data ShapeRender = ShapeRender Shape Color Position
instance Extractable ShapeRender where extract = extract_2r1d ShapeRender

instance Renderable World where
  render world = Pictures (mapStore renderShapes world ++ mapStore renderSprites world) where
    renderShapes (ShapeRender s c (Position (x, y))) = translate x y $ color c $ render s
    renderSprites :: (Picture, Position) -> Picture
    renderSprites (p, Position (x, y)) = translate x y $ p
