module ReduxGame.Game
  ( Renderable
  , startGame
  , initialiseGame
  , module ReduxGame.Redux
  ) where

import Data.Typeable

import Graphics.Gloss.Interface.IO.Game

import ReduxGame.Renderer.Renderable
import ReduxGame.Redux
import ReduxGame.Exit

startGame :: (Typeable w, Renderable w) => w -> Redux w -> IO ()
startGame world redux = do
  let redux' = redux |:: exitRedux 'q'
  playIO FullScreen black 30 world (pure . render) (reduxListen redux') (reduxUpdate redux')

initialiseGame :: (Typeable w, Renderable w) => w -> Redux w -> Events () -> IO ()
initialiseGame world redux initialiser = do
  world' <- reduxDo redux initialiser world
  startGame world' redux
