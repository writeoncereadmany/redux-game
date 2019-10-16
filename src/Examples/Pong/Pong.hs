module Examples.Pong.Pong where

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.WorldShapeRenderer
import ReduxGame.Components
import ReduxGame.Collisions

import Examples.Pong.Bat
import Examples.Pong.Ball
import Examples.Pong.Wall
import Examples.Pong.Goal

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

checkForGoals :: TimeStep -> World -> Events ()
checkForGoals = fireOnCollision Goal Ball goalScored where
  goalScored :: EntityId -> EntityId -> Events ()
  goalScored goal_id ball_id = fireEvent $ GoalScored goal_id ball_id

resetBallOnScore :: GoalScored -> World -> Events ()
resetBallOnScore (GoalScored goal_id ball_id) world = do
  destroy ball_id
  spawn $ ball 0 0 700 450


pongRedux :: Redux World
pongRedux = worldRedux
        |:: batRedux
        |:: collisionRedux
        |!> checkForGoals
        |!> resetBallOnScore
        |$> applyVelocity
