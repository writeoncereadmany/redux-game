module Examples.Pong.Pong
  ( pongRedux
  , newPong
  , initialisePong
  ) where

import Control.Lens

import ReduxGame.Renderer.Renderable
import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.WorldShapeRenderer
import ReduxGame.Components
import ReduxGame.Collisions
import ReduxGame.Timer

import Examples.Pong.Levels.Court
import Examples.Pong.Controllers.Bats
import Examples.Pong.Controllers.Scoring

data Pong = Pong
  { _world :: World
  , _timer :: Timer
  }

makeLenses ''Pong

instance Renderable Pong where
  render pong = render (pong ^. world)

newPong = Pong newWorld newTimer


pongRedux :: Redux Pong
pongRedux = connect world entityRedux
        |:: connect timer timerRedux

entityRedux :: Redux World
entityRedux = worldRedux
          |:: batRedux
          |:: collisionRedux
          |:: scoringRedux
          |$> applyVelocity
