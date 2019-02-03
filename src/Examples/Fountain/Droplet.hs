module Examples.Fountain.Droplet where

import Control.Monad.Trans
import System.Random

import ReduxGame.Entities.Entity
import ReduxGame.Entities.EntityRedux
import ReduxGame.Timer
import ReduxGame.Redux
import ReduxGame.Shape.Shape

import Graphics.Gloss

data Position = Position Float Float deriving Component
data Velocity = Velocity Float Float deriving Component
data Acceleration = Acceleration Float Float deriving Component

instance Component Shape
instance Component Color

transBlue :: Color
transBlue = makeColor 0.3 0.6 1 0.6

droplet :: Float -> Float -> Entity
droplet x y = entity
          <-+ Position 0 (-600)
          <-+ Velocity x (y + 2400)
          <-+ Acceleration 0 (-2800)
          <-+ transBlue
          <-+ rectangle (-5, -5) (10, 10)

createDroplet :: Events ()
createDroplet = do
  x <- liftIO $ getStdRandom $ randomR (-400, 400)
  y <- liftIO $ getStdRandom $ randomR (-200, 200)
  spawnThen (droplet x y) (await 2 . destroy)

integrate :: TimeStep -> (Position, Velocity, Acceleration) -> (Position, Velocity)
integrate (TimeStep t) ((Position x y), (Velocity dx dy), (Acceleration ddx ddy)) =
  let dx' = dx + ddx * t
      dy' = dy + ddy * t
      x' = x + dx' * t
      y' = y + dy' * t
   in (Position x' y', Velocity dx' dy')
