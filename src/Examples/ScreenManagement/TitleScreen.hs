module Examples.ScreenManagement.TitleScreen where

import Graphics.Gloss

import ReduxGame.Renderer.Renderable
import ReduxGame.Redux

import Examples.ScreenManagement.Transitions
import Examples.ScreenManagement.RenderTextLines

data TitleScreen = TitleScreen

instance Renderable TitleScreen where
  render TitleScreen = translate (-500) 300
                     $ color white
                     $ textLines ["Title screen", "Press G to play", "Press H for high scores"] 200

titleScreenRedux :: Redux TitleScreen
titleScreenRedux = redux
               |=> onButton 'g' (fireEvent StartGame)
               |=> onButton 'h' (fireEvent ViewScores)
