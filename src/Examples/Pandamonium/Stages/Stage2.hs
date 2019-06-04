module Examples.Pandamonium.Stages.Stage2 where

import ReduxGame.Entities
import Examples.Pandamonium.Assets.PandaAssets
import Examples.Pandamonium.Entities.Wall
import Examples.Pandamonium.Entities.Coin
import Examples.Pandamonium.Entities.Hero

stage2 :: PandaAssets -> [Entity]
stage2 assets =
  [ wall (-1300, -800) (2600, 50)
  , wall (-1300, 750) (2600, 50)
  , wall (-1300, -800) (50, 1600)
  , wall (1250, -800) (50, 1600)
  , wall (-900, -800) (500, 550)
  , wall (400, -800) (500, 550)
  , wall (-250, 100) (500, 50)

  , coin assets (-650, -200)
  , coin assets (650, -200)
  , hero (0, 0)
  ]
