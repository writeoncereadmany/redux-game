module Examples.Pong.Controllers.Scoring where

import ReduxGame.Redux
import ReduxGame.Collisions
import ReduxGame.Entities
import ReduxGame.Timer

import Examples.Pong.Entities.Ball
import Examples.Pong.Entities.Goal

data GoalScored = GoalScored EntityId EntityId deriving ReduxEvent

checkForGoals :: TimeStep -> World -> Events World
checkForGoals = fireOnCollision' Goal Ball GoalScored

resetBallOnScore :: GoalScored -> Events ()
resetBallOnScore (GoalScored goal_id ball_id) = do
  destroy ball_id
  await 2 $ spawn $ ball 0 0 700 450

scoringRedux :: Redux World
scoringRedux = redux
           |=> checkForGoals
           |!> resetBallOnScore
