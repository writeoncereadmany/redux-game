module Examples.Pandamonium.Events where

import ReduxGame.Redux

data JumpEvent = JumpEvent deriving ReduxEvent
data LevelComplete = LevelComplete deriving ReduxEvent
data GameOver = GameOver deriving ReduxEvent
data Pulse = Pulse deriving ReduxEvent
