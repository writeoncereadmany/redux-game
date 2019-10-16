module Examples.Pong.Pong where

import Control.Lens

import ReduxGame.Renderer.Renderable
import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.WorldShapeRenderer
import ReduxGame.Components
import ReduxGame.Collisions
import ReduxGame.Timer

import Examples.Pong.Bat
import Examples.Pong.Ball
import Examples.Pong.Wall
import Examples.Pong.Goal

data Pong = Pong
  { _world :: World
  , _timer :: Timer
  }

makeLenses ''Pong

instance Renderable Pong where
  render pong = render (pong ^. world)

newPong = Pong newWorld newTimer

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

checkForGoals :: TimeStep -> World -> Events World
checkForGoals = fireOnCollision' Goal Ball GoalScored

resetBallOnScore :: GoalScored -> Events ()
resetBallOnScore (GoalScored goal_id ball_id) = do
  destroy ball_id
  await 2 $ spawn $ ball 0 0 700 450

pongRedux :: Redux Pong
pongRedux = connect world entityRedux
        |:: connect timer timerRedux

entityRedux :: Redux World
entityRedux = worldRedux
          |:: batRedux
          |:: collisionRedux
          |=> checkForGoals
          |!> resetBallOnScore
          |$> applyVelocity
