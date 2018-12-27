module Examples.ScreenManagement.ScreenManagement
 ( session
 , sessionRedux
 , Examples.ScreenManagement.LoadingScreen.initialiseLoadingScreen
 ) where

import Graphics.Gloss
import Control.Lens

import ReduxGame.Redux
import ReduxGame.Timer
import ReduxGame.Renderer.Renderable

import Examples.ScreenManagement.LoadingScreen

data TitleScreen = TitleScreen
data GameScreen = GameScreen
data HighScoreScreen = HighScoreScreen

instance Renderable TitleScreen where
  render TitleScreen = color white $ text "Title screen"

instance Renderable GameScreen where
  render GameScreen = color green $ text "Game screen"

instance Renderable HighScoreScreen where
  render HighScoreScreen = color red $ text "High score screen"

data Screen = Loading LoadingScreen
             | Title TitleScreen
             | Game GameScreen
             | HighScores HighScoreScreen

makePrisms ''Screen

toTitleScreen :: FinishedLoading -> Screen -> Screen
toTitleScreen _ (Loading _) = Title TitleScreen

screenRedux :: Redux Screen
screenRedux = redux
          |:: connect _Loading loadingScreenRedux
          |-> toTitleScreen


instance Renderable Screen where
  render (Loading l) = render l
  render (Title t) = render t
  render (Game g) = render g
  render (HighScores h) = render h

data Session = Session
  { _screen :: Screen
  , _timer :: Timer
  }

makeLenses ''Session

instance Renderable Session where
  render s = render (s ^. screen)

session :: Session
session = Session
  { _screen = Loading $ LoadingScreen [ "Loading: " ] False
  , _timer = newTimer
  }

sessionRedux :: Redux Session
sessionRedux = redux
           |:: connect timer timerRedux
           |:: connect screen screenRedux
