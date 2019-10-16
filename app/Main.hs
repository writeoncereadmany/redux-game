module Main where

import System.Environment
import System.Exit

import ReduxGame.Game
import ReduxGame.Entities

import Examples.Orrery.Orrery
import Examples.ScreenManagement.ScreenManagement
import Examples.Balls.Balls
import Examples.Fountain.Fountain
import Examples.Pandamonium.Pandamonium
import Examples.Pandamonium.Assets.PandaAssets
import Examples.Pong.Pong

main :: IO ()
main = getArgs >>= run where
  run ["orrery"] = startGame orrery orreryRedux
  run ["screens"] = initialiseGame session sessionRedux initialiseLoadingScreen
  run ["balls"] = initialiseGame balls ballsRedux initialiseBalls
  run ["fountain"] = initialiseGame fountain fountainRedux initialiseFountain
  run ["pong"] = initialiseGame newPong pongRedux initialisePong
  run ["panda"] = do
    assets <- loadAssets
    initialiseGame (initialPandas assets) pandaGameRedux initialisePandas
  run _ = exitFailure
