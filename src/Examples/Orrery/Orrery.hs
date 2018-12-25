module Examples.Orrery.Orrery where

import Graphics.Gloss
import Control.Lens

import ReduxGame.Redux
import ReduxGame.Timer
import ReduxGame.Renderer.Renderable

data Orrery = Orrery
  { _timer :: Timer
  }

makeLenses ''Orrery

orrery :: Orrery
orrery = Orrery newTimer

instance Renderable Orrery where
  render _ = color yellow $ circleSolid 50

orreryRedux :: Redux Orrery
orreryRedux = redux
          |+> connect timer timerRedux
