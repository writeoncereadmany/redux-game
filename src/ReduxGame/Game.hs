module ReduxGame.Game
  ( Renderable
  , startGame
  , module ReduxGame.Redux
  ) where

import Graphics.Gloss.Interface.IO.Game

import ReduxGame.Renderer.Renderable
import ReduxGame.Redux
import ReduxGame.Exit

startGame :: Renderable w => w -> Redux w -> IO ()
startGame world redux = do
  let redux' = redux |+> exitRedux 'q'
  playIO FullScreen black 60 world (pure . render) (reduxListen redux') (reduxUpdate redux')
