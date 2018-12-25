module ReduxGame.Renderer.Renderable where

import Graphics.Gloss (Picture)

class Renderable w where
  render :: w -> Picture
