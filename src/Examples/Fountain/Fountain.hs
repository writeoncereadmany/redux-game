module Examples.Fountain.Fountain where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import System.Random

import ReduxGame.Entities.Entities
import ReduxGame.Entities.Entity
import ReduxGame.Entities.EntityRedux
import ReduxGame.Shape.Shape
import ReduxGame.Redux
import ReduxGame.Timer
import ReduxGame.Renderer.Renderable
import ReduxGame.Renderer.ShapeRenderer
import Graphics.Gloss hiding (circle)

type World = ComponentStore ListStore

data Position = Position Float Float deriving Component
data Velocity = Velocity Float Float deriving Component
data Acceleration = Acceleration Float Float deriving Component

instance Component Shape
instance Component Color

data Fountain = Fountain
  { _world :: World
  , _timer :: Timer
  }

makeLenses ''Fountain

transBlue :: Color
transBlue = makeColor 0.3 0.6 1 0.4

fountain :: Fountain
fountain = Fountain emptyComponents newTimer

instance Renderable Fountain where
  render fountain = Pictures $ foldStore render' (fountain ^. world) where
    render' :: (Position, Shape, Color) -> Picture
    render' ((Position x y), s, c) = translate x y $ color c $ render s

initialiseFountain :: Events ()
initialiseFountain = schedule 0.05 (times 30 createShiny)

times :: Monad m => Int -> m () -> m ()
times n action = do
  replicateM n action
  return ()

particle :: Float -> Float -> Entity
particle x y = entity
           <-+ Position 0 (-600)
           <-+ Velocity x (y + 2400)
           <-+ Acceleration 0 (-2800)
           <-+ transBlue
           <-+ rectangle (-5, -5) (10, 10)

createShiny :: Events ()
createShiny = do
  x <- liftIO $ getStdRandom $ randomR (-400, 400)
  y <- liftIO $ getStdRandom $ randomR (-200, 200)
  spawnThen (particle x y) (await 2 . destroy)

integrate :: TimeStep -> (Position, Velocity, Acceleration) -> (Position, Velocity)
integrate (TimeStep t) ((Position x y), (Velocity dx dy), (Acceleration ddx ddy)) =
  let dx' = dx + ddx * t
      dy' = dy + ddy * t
      x' = x + dx' * t
      y' = y + dy' * t
   in (Position x' y', Velocity dx' dy')

worldRedux :: Redux World
worldRedux = entityRedux
         |$> integrate

fountainRedux :: Redux Fountain
fountainRedux = redux
            |:: connect world worldRedux
            |:: connect timer timerRedux
