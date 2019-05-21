module ReduxGame.Renderer.ShapeRenderer where

import qualified Graphics.Gloss as G

import ReduxGame.Renderer.Renderable
import ReduxGame.Shape

instance Renderable Shape where
  render (Circle (x, y) r) = G.translate x y $ G.circleSolid r
  render (Polygon points _) = G.polygon points
