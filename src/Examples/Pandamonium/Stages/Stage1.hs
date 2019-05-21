module Examples.Pandamonium.Stages.Stage1 where

import ReduxGame.Entities
import Examples.Pandamonium.Entities.Wall
import Examples.Pandamonium.Entities.Coin
import Examples.Pandamonium.Entities.Hero

stage1 :: [Entity]
stage1 =
  [ wall (-1300, -800) (2600, 50)
  , wall (-1300, 750) (2600, 50)
  , wall (-1300, -800) (50, 1600)
  , wall (1250, -800) (50, 1600)
  , wall (-900, -300) (500, 50)
  , wall (400, -300) (500, 50)
  , wall (-250, 100) (500, 50)

  , coin (0, 250)
  , hero (0, 0)
  ]
