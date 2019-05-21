module ReduxGame.Exit where

import System.Exit
import Control.Monad
import Control.Monad.Trans
import Graphics.Gloss.Interface.IO.Game

import ReduxGame.InputEvents
import ReduxGame.Redux

quit :: Events ()
quit = liftIO exitSuccess

listenForQuit :: Char -> Event -> Events ()
listenForQuit button event = when (isKeyPress button event) quit

exitRedux :: Char -> Redux a
exitRedux quitButton = redux
                   |!> listenForQuit quitButton
