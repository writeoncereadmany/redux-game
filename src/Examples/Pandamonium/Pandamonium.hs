module Examples.Pandamonium.Pandamonium where

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Components
import ReduxGame.Collisions

import Examples.Pandamonium.Controllers.Pickups
import Examples.Pandamonium.Controllers.HeroMovement

import Examples.Pandamonium.Stages.Stage1

pandas = newWorld

initialisePandas :: Events ()
initialisePandas = do
  traverse spawn stage1
  return ()


pandaGameRedux :: Redux World
pandaGameRedux = worldRedux
             |$> integrate
             |:: collisionRedux
             |:: pickupRedux
             |:: heroRedux
