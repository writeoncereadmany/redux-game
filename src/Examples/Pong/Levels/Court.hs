module Examples.Pong.Levels.Court where

import ReduxGame.Redux
import ReduxGame.Entities

import Examples.Pong.Entities.Ball
import Examples.Pong.Entities.Bat
import Examples.Pong.Entities.Goal
import Examples.Pong.Entities.Wall

initialisePong :: Events ()
initialisePong = do
  spawn $ bat (-1100) 0 'a' 'z'
  spawn $ bat 1100 0 '\'' '/'
  spawn $ ball 0 0 700 450
  spawn $ goal (-1180) (-680) 30 1360
  spawn $ goal (1150) (-680) 30 1360
  spawn $ wall (-1200) (-700) 2400 20
  spawn $ wall (-1200) (680) 2400 20
  spawn $ wall (-1200) (-700) 20 1400
  spawn $ wall (1180) (-700) 20 1400
