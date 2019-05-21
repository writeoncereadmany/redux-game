module Examples.Pandamonium.Labels where

import ReduxGame.Entities

-- controller aspect markers
data Horizontal = Horizontal
data Jump = Jump

-- entity type/aspect markers
data Coin = Coin deriving Component
data Hero = Hero deriving Component
data FeelsGravity = FeelsGravity deriving Component
