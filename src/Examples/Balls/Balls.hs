module Examples.Balls.Balls where

import ReduxGame.Entities
import ReduxGame.Shape
import ReduxGame.Redux
import ReduxGame.Renderer.Renderable
import ReduxGame.Renderer.ShapeRenderer
import ReduxGame.Components
import ReduxGame.Collisions
import Graphics.Gloss hiding (circle)

import Examples.Balls.Paddle
import Examples.Balls.Ball
import Examples.Balls.Wall

balls :: World
balls = newWorld

initialiseBalls :: Events ()
initialiseBalls = do
  sequence $ spawn <$> [ ball (x, y) (400, 1200) (0, -2000) | x <- [-600, -500 .. 600], y <- [-200, -100 .. 200]]
  spawn $ wall (-1100, -700) (2200, 150)
  spawn $ wall (-1100, 550) (2200, 150)
  spawn $ wall (-1100, -600) (150, 1200)
  spawn $ wall (950, -600) (150, 1200)
  spawn $ paddle (-100, -300) (200, 50)

data ShapeRender = ShapeRender Shape Color Position
instance Extractable ShapeRender where extract = extract_2r1d ShapeRender

instance Renderable World where
  render world = Pictures $ foldStore render' world where
    render' (ShapeRender s c (Position (x, y))) = translate x y $ color c $ render s

ballsRedux :: Redux World
ballsRedux = worldRedux
         |$> integrate
         |:: collisionRedux
         |:: paddleRedux
