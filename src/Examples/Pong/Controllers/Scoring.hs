module Examples.Pong.Controllers.Scoring where

import System.Random
import Control.Monad.Trans

import ReduxGame.Redux
import ReduxGame.Collisions
import ReduxGame.Components
import ReduxGame.Entities
import ReduxGame.Timer
import ReduxGame.Entities.Store.ComponentStore

import Examples.Pong.Entities.Ball
import Examples.Pong.Entities.Goal
import Examples.Pong.Entities.Particle

data GoalScored = GoalScored EntityId EntityId deriving ReduxEvent

checkForGoals :: TimeStep -> World -> Events World
checkForGoals = fireOnCollision' Goal Ball GoalScored

resetBallOnScore :: GoalScored -> World -> Events World
resetBallOnScore (GoalScored goal_id ball_id) world = do
  generateParticles ball_id world
  destroy ball_id
  await 2 $ spawn $ ball 0 0 700 450
  return world

generateParticles :: EntityId -> World -> Events ()
generateParticles ball_id world = case getComponent ball_id world of
  (Just (Position (x, y))) -> times 6 (spawnParticle x y)
  Nothing -> return ()

spawnParticle :: Float -> Float -> Events ()
spawnParticle x y = do
  angle <- randomFloat 0 (2*pi)
  let dx = sin angle * 400
  let dy = cos angle * 400
  spawnThen (particle x y dx dy) (await 3 . destroy) where
    randomFloat :: Float -> Float -> Events Float
    randomFloat min max = do
      val <- liftIO randomIO
      return $ val * (max - min) + min

times :: Monad m => Int -> m () -> m ()
times n f = do
  sequence (replicate n f)
  return ()

scoringRedux :: Redux World
scoringRedux = redux
           |=> checkForGoals
           |=> resetBallOnScore
