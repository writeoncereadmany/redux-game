module Main where

import ReduxGame.Game
import Examples.Orrery.Orrery

main :: IO ()
main = startGame orrery orreryRedux
