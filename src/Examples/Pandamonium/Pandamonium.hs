module Examples.Pandamonium.Pandamonium where

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Components
import ReduxGame.Collisions

import Examples.Pandamonium.Controllers.Pickups
import Examples.Pandamonium.Controllers.HeroMovement
import Examples.Pandamonium.Controllers.Controls
import Examples.Pandamonium.Controllers.Physics

import Examples.Pandamonium.Stages.Stage1
import Examples.Pandamonium.Stages.Stage2

pandas = newWorld

initialisePandas :: Events ()
initialisePandas = do
  traverse spawn stage2
  return ()


pandaGameRedux :: Redux World
pandaGameRedux = worldRedux
             |:: pickupRedux
             |:: heroRedux
             |:: controlRedux
             |:: physicsRedux
