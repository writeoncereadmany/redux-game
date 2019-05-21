module ReduxGame.WorldShapeRenderer where

import Graphics.Gloss

import ReduxGame.Renderer.Renderable
import ReduxGame.Renderer.ShapeRenderer
import ReduxGame.Shape
import ReduxGame.Components
import ReduxGame.Entities

data ShapeRender = ShapeRender Shape Color Position
instance Extractable ShapeRender where extract = extract_2r1d ShapeRender

instance Renderable World where
  render world = Pictures $ foldStore render' world where
    render' (ShapeRender s c (Position (x, y))) = translate x y $ color c $ render s
