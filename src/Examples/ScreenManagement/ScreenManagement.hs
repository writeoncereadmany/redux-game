module Examples.ScreenManagement.ScreenManagement where

import Graphics.Gloss
import Control.Lens

import ReduxGame.Redux
import ReduxGame.Renderer.Renderable

data LoadingScreen = LoadingScreen
data TitleScreen = TitleScreen
data GameScreen = GameScreen
data HighScoreScreen = HighScoreScreen

instance Renderable LoadingScreen where
  render LoadingScreen = color yellow $ text "Loading screen "

loadingScreenRedux :: Redux LoadingScreen
loadingScreenRedux = redux

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

screenRedux :: Redux Screen
screenRedux = redux
          |+> connect _Loading loadingScreenRedux


instance Renderable Screen where
  render (Loading l) = render l
  render (Title t) = render t
  render (Game g) = render g
  render (HighScores h) = render h

data Session = Session Screen

instance Renderable Session where
  render (Session s) = render s

session :: Session
session = Session $ Loading LoadingScreen

sessionRedux :: Redux Session
sessionRedux = redux
