module Examples.Pandamonium.Stages.Stage1 where

import ReduxGame.Entities
import Examples.Pandamonium.Assets.PandaAssets
import Examples.Pandamonium.Entities.Wall
import Examples.Pandamonium.Entities.Coin
import Examples.Pandamonium.Entities.Hero

stage1 :: PandaAssets -> [Entity]
stage1 assets =
  [ wall (-180, -100) (360, 5)
  , wall (-180, 95) (360, 5)
  , wall (-180, -100) (5, 200)
  , wall (175, -100) (5, 200)
  , wall (-140, -60) (40, 5)
  , wall (100, -60) (40, 5)
  , wall (-80, -25) (40, 5)
  , wall (40, -25) (40, 5)

  , coin assets (0, 40)
  , hero assets (0, 0)
  ]
