module ReduxGame.Exit where

import System.Exit
import Control.Monad
import Control.Monad.Trans
import Graphics.Gloss.Interface.IO.Game

import ReduxGame.InputEvents
import ReduxGame.Redux
import ReduxGame.Monad

data Exit = Exit deriving ReduxEvent

exit :: Exit -> Events ()
exit _ = liftIO exitSuccess

listenForQuit :: Char -> Event -> Events ()
listenForQuit button event = when (isKeyPress button event) $ fireEvent Exit

exitRedux :: Char -> Redux a
exitRedux quitButton = redux
                   |=> doing (listenForQuit quitButton)
                   |=> doing exit
