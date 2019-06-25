module ReduxGame.Exit where

import System.Exit
import Control.Monad
import Control.Monad.Trans
import Data.Typeable
import Graphics.Gloss.Interface.IO.Game

import ReduxGame.InputEvents
import ReduxGame.Redux

quit :: Events ()
quit = liftIO exitSuccess

listenForQuit :: Char -> Event -> Events ()
listenForQuit button event = when (isKeyPress button event) quit

exitRedux :: Typeable a => Char -> Redux a
exitRedux quitButton = redux
                   |!> listenForQuit quitButton
