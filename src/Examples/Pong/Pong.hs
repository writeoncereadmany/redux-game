module Examples.Pong.Pong where

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.WorldShapeRenderer
import ReduxGame.Components
import ReduxGame.Collisions

import Examples.Pong.Bat
import Examples.Pong.Ball
import Examples.Pong.Wall

initialisePong :: Events ()
initialisePong = do
  spawn $ bat (-1100) 0 'a' 'z'
  spawn $ bat 1100 0 '\'' '/'
  spawn $ ball 0 0 700 450
  spawn $ wall (-1200) (-700) 2400 20
  spawn $ wall (-1200) (680) 2400 20
  spawn $ wall (-1200) (-700) 20 1400
  spawn $ wall (1180) (-700) 20 1400


pongRedux :: Redux World
pongRedux = batRedux
        |$> applyVelocity
        |:: collisionRedux
