module Examples.Orrery.Orrery where

import Graphics.Gloss

import ReduxGame.Redux
import ReduxGame.Renderer.Renderable

data Orrery = Orrery

orrery :: Orrery
orrery = Orrery

instance Renderable Orrery where
  render _ = color yellow $ circleSolid 50

orreryRedux :: Redux Orrery
orreryRedux = redux
