module Examples.Fountain.Fountain where

import Control.Lens
import Control.Monad

import ReduxGame.Entities.Entities
import ReduxGame.Entities.Entity
import ReduxGame.Entities.EntityRedux
import ReduxGame.Redux
import ReduxGame.Timer
import ReduxGame.Renderer.Renderable
import ReduxGame.Renderer.ShapeRenderer
import Graphics.Gloss
import ReduxGame.Shape.Shape

import Examples.Fountain.Droplet

type World = ComponentStore MapStore

data Fountain = Fountain
  { _world :: World
  , _timer :: Timer
  }

makeLenses ''Fountain

fountain :: Fountain
fountain = Fountain emptyComponents newTimer

instance Renderable Fountain where
  render fountain = Pictures $ foldStore render' (fountain ^. world) where
    render' :: (Position, Shape, Color) -> Picture
    render' ((Position x y), s, c) = translate x y $ color c $ render s

initialiseFountain :: Events ()
initialiseFountain = schedule 0.05 (times 50 createDroplet)

times :: Monad m => Int -> m () -> m ()
times n action = do
  replicateM n action
  return ()

worldRedux :: Redux World
worldRedux = entityRedux
         |$> integrate

fountainRedux :: Redux Fountain
fountainRedux = redux
            |:: connect world worldRedux
            |:: connect timer timerRedux
