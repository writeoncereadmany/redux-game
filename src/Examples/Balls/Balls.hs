module Examples.Balls.Balls where

import ReduxGame.Entities.Entities
import ReduxGame.Entities.Entity
import ReduxGame.Entities.EntityRedux
import ReduxGame.Entities.Store.Variadics
import ReduxGame.Shape.Shape
import ReduxGame.Redux
import ReduxGame.Renderer.Renderable
import ReduxGame.Renderer.ShapeRenderer
import ReduxGame.Components.Components
import ReduxGame.Collisions.CollisionEvents
import ReduxGame.Collisions.CollisionRedux
import Graphics.Gloss hiding (circle)

import Examples.Balls.Paddle
import Examples.Balls.Ball
import Examples.Balls.Wall

type World = ComponentStore MapStore

balls :: World
balls = emptyComponents

initialiseBalls :: Events ()
initialiseBalls = do
  sequence $ spawn <$> [ ball (x, y) (400, 1200) (0, -2000) | x <- [-600, -500 .. 600], y <- [-200, -100 .. 200]]
  spawn $ wall (-1100, -700) (2200, 150)
  spawn $ wall (-1100, 550) (2200, 150)
  spawn $ wall (-1100, -600) (150, 1200)
  spawn $ wall (950, -600) (150, 1200)
  spawn $ paddle (-100, -300) (200, 50)

integrate :: TimeStep -> (Position, Velocity, Acceleration) -> (Position, Velocity)
integrate (TimeStep t) ((Position (x, y)), (Velocity (dx, dy)), (Acceleration (ddx, ddy))) =
  let (dx', dy') = (dx + ddx * t, dy + ddy * t)
      (x', y') = (x + dx' * t, y + dy' * t)
   in (Position (x', y'), Velocity (dx', dy'))

data ShapeRender = ShapeRender Shape Color Position
instance Extractable ShapeRender where
  extract = extract_2r1d ShapeRender

instance Renderable World where
  render world = Pictures $ foldStore render' world where
    render' :: ShapeRender -> Picture
    render' (ShapeRender s c (Position (x, y))) = translate x y $ color c $ render s

ballsRedux :: Redux World
ballsRedux = entityRedux
         |$> integrate
         |:: collisionRedux
         |:: paddleRedux
