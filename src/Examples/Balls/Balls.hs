module Examples.Balls.Balls where

import ReduxGame.Entities.Entities
import ReduxGame.Entities.Entity
import ReduxGame.Entities.EntityRedux
import ReduxGame.Shape.Shape
import ReduxGame.Redux
import ReduxGame.Renderer.Renderable
import ReduxGame.Renderer.ShapeRenderer
import Graphics.Gloss hiding (circle)

type World = ComponentStore ListStore

balls :: World
balls = emptyComponents

data Position = Position Float Float deriving Component
data Velocity = Velocity Float Float deriving Component

instance Component Shape
instance Component Color

initialiseBalls :: Events ()
initialiseBalls = spawn $ entity <-+ Position 0 0 <-+ Velocity 200 0 <-+ circle 0 50 <-+ yellow

integrate :: TimeStep -> (Position, Velocity) -> Only Position
integrate (TimeStep t) ((Position x y), (Velocity dx dy)) = Only $ Position (x + dx * t) (y + dy * t)

killOutOfRange :: TimeStep -> Tagged (Only Position) -> Events ()
killOutOfRange _ (Tagged entId (Only (Position x y))) = if x > 300
  then destroy entId
  else return ()

instance Renderable World where
  render world = Pictures $ foldStore render' world where
    render' :: (Position, Shape, Color) -> Picture
    render' ((Position x y), s, c) = translate x y $ color c $ render s

ballsRedux :: Redux World
ballsRedux = entityRedux
         |$> integrate
         |*> killOutOfRange
