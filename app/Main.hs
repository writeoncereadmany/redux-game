module Main where

import System.Environment

import ReduxGame.Game
import Examples.Orrery.Orrery
import Examples.ScreenManagement.ScreenManagement
import Examples.Balls.Balls
import Examples.Fountain.Fountain

main :: IO ()
main = getArgs >>= run where
  run ["orrery"] = startGame orrery orreryRedux
  run ["screens"] = initialiseGame session sessionRedux initialiseLoadingScreen
  run ["balls"] = initialiseGame balls ballsRedux initialiseBalls
  run ["fountain"] = initialiseGame fountain fountainRedux initialiseFountain
