module Examples.Pandamonium.Stages.Stage2 where

import ReduxGame.Entities
import Examples.Pandamonium.Assets.PandaAssets
import Examples.Pandamonium.Entities.Wall
import Examples.Pandamonium.Entities.Coin
import Examples.Pandamonium.Entities.Hero

stage2 :: PandaAssets -> [Entity]
stage2 assets =
  [ wall (-180, -100) (360, 5)
  , wall (-180, 95) (360, 5)
  , wall (-180, -100) (5, 200)
  , wall (175, -100) (5, 200)

  , coin assets (-80, -25)
  , coin assets (80, -25)
  , hero assets (0, 0)
  ]
