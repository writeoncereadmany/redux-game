module Examples.Pandamonium.Pandamonium where

import ReduxGame.Redux
import ReduxGame.Entities

import Examples.Pandamonium.Entities.Panda (pandaRedux)

import Examples.Pandamonium.Stages.Stage1

pandas = newWorld

initialisePandas :: Events ()
initialisePandas = do
  traverse spawn stage1
  return ()


pandaGameRedux :: Redux World
pandaGameRedux = worldRedux
             |:: pandaRedux
