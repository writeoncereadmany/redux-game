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

instance Component Shape
instance Component Color

createBall :: Entities ()
createBall = do
  create $ entity <-+ Position 0 0 <-+ circle 0 50 <-+ yellow
  return ()

initialBalls :: World
initialBalls = updateState createBall world

instance Renderable World where
  render world = Pictures $ map3 render' world where
    render' :: (Position, Shape, Color) -> Picture
    render' ((Position x y), s, c) = translate x y $ color c $ render s

ballsRedux :: Redux World
ballsRedux = redux
