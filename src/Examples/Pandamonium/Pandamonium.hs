module Examples.Pandamonium.Pandamonium where

import ReduxGame.Redux
import ReduxGame.Entities

import Examples.Pandamonium.Entities.Wall

pandas = newWorld

initialisePandas :: Events ()
initialisePandas = do
  spawn $ wall (-1300, -800) (2600, 50)
  spawn $ wall (-1300, 750) (2600, 50)
  spawn $ wall (-1300, -800) (50, 1600)
  spawn $ wall (1250, -800) (50, 1600)
  spawn $ wall (-900, -300) (500, 50)
  spawn $ wall (400, -300) (500, 50)
  spawn $ wall (-250, 100) (500, 50)


pandaRedux :: Redux World
pandaRedux = worldRedux
