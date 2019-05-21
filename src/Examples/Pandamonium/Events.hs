module Examples.Pandamonium.Events where

import ReduxGame.Redux

data JumpEvent = JumpEvent deriving ReduxEvent
data LevelComplete = LevelComplete deriving ReduxEvent
