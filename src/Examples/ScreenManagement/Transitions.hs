module Examples.ScreenManagement.Transitions where

import ReduxGame.Redux

data ToTitleScreen = ToTitleScreen deriving ReduxEvent
data StartGame = StartGame deriving ReduxEvent
data ViewScores = ViewScores deriving ReduxEvent
