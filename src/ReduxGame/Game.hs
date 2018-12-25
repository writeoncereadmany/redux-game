module ReduxGame.Game
  ( Renderable
  , startGame
  , module ReduxGame.Redux 
  ) where

import Graphics.Gloss.Interface.IO.Game
import ReduxGame.Redux

class Renderable w where
  render :: w -> Picture

startGame :: Renderable w => w -> Redux w -> (w -> Picture) -> IO ()
startGame world redux renderer = playIO FullScreen black 60 world (pure . render) (reduxListen redux) (reduxUpdate redux)
