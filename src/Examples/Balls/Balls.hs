module Examples.Balls.Balls where

import ReduxGame.Entities.Entities
import ReduxGame.Entities.Entity
import ReduxGame.Shape.Shape
import ReduxGame.Redux
import ReduxGame.Renderer.Renderable
import ReduxGame.Renderer.ShapeRenderer
import Graphics.Gloss hiding (circle)

type World = ComponentStore ListStore

world :: World
world = emptyComponents

data Position = Position Float Float deriving Component
data Velocity = Velocity Float Float deriving Component

instance Component Shape
instance Component Color

createBall :: Entities ()
createBall = do
  create $ entity <-+ Position 0 0 <-+ Velocity 200 0 <-+ circle 0 50 <-+ yellow
  return ()

initialBalls :: World
initialBalls = updateState createBall world

integrate :: TimeStep -> (Position, Velocity) -> Only Position
integrate (TimeStep t) ((Position x y), (Velocity dx dy)) = Only $ Position (x + dx * t) (y + dy * t)

infixl 1 |$>
(|$>) :: (ReduxEvent a, Extractable b, Updatable c)
      => Redux World
      -> (a -> b -> c)
      -> Redux World
redux |$> f = redux |-> apply . f

instance Renderable World where
  render world = Pictures $ foldStore render' world where
    render' :: (Position, Shape, Color) -> Picture
    render' ((Position x y), s, c) = translate x y $ color c $ render s

ballsRedux :: Redux World
ballsRedux = redux
         |$> integrate
