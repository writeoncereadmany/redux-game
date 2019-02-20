module Examples.Balls.Balls where

import ReduxGame.Entities.Entities
import ReduxGame.Entities.Entity
import ReduxGame.Entities.EntityRedux
import ReduxGame.Shape.Shape
import ReduxGame.Redux
import ReduxGame.Renderer.Renderable
import ReduxGame.Renderer.ShapeRenderer
import ReduxGame.Components.Components
import ReduxGame.Collisions.CollisionRedux
import Graphics.Gloss hiding (circle)

import Examples.Balls.Ball
import Examples.Balls.Wall

type World = ComponentStore MapStore

balls :: World
balls = emptyComponents

initialiseBalls :: Events ()
initialiseBalls = do
  spawn $ ball (0, 0) (400, 250)
  spawn $ wall (-1000, -600) (2000, 50)
  spawn $ wall (-1000, 550) (2000, 50)
  spawn $ wall (-1000, -600) (50, 1200)
  spawn $ wall (950, -600) (50, 1200)


integrate :: TimeStep -> (Position, Velocity) -> Only Position
integrate (TimeStep t) ((Position (x, y)), (Velocity (dx, dy))) = Only $ Position (x + dx * t, y + dy * t)

instance Renderable World where
  render world = Pictures $ foldStore render' world where
    render' :: (Position, Shape, Color) -> Picture
    render' ((Position (x, y)), s, c) = translate x y $ color c $ render s

ballsRedux :: Redux World
ballsRedux = entityRedux
         |$> integrate
         |:: collisionRedux
