module ReduxGame.Exit where

import System.Exit
import Control.Monad
import Control.Monad.Trans
import Graphics.Gloss.Interface.IO.Game

import ReduxGame.Redux
import ReduxGame.Monad

data Exit = Exit deriving ReduxEvent

exit :: Exit -> Events ()
exit _ = liftIO exitSuccess

listenForQuit :: Char -> Event -> Events ()
listenForQuit button (EventKey (Char pressed) _ _ _) = when (button == pressed) $ fireEvent Exit
listenForQuit _ _ = return ()

exitRedux :: Char -> Redux a
exitRedux quitButton = redux
                   |=> doing (listenForQuit quitButton)
                   |=> doing exit
