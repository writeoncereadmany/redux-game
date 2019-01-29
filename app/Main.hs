module Main where

import System.Environment

import ReduxGame.Game
import Examples.Orrery.Orrery
import Examples.ScreenManagement.ScreenManagement
import Examples.Balls.Balls

main :: IO ()
main = getArgs >>= run where
  run ["orrery"] = startGame orrery orreryRedux
  run ["screens"] = initialiseGame session sessionRedux initialiseLoadingScreen
  run ["balls"] = startGame initialBalls ballsRedux
