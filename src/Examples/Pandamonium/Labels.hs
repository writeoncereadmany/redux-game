module Examples.Pandamonium.Labels where

import ReduxGame.Entities

-- controller aspect markers
data Horizontal = Horizontal
data Jump = Jump

-- entity type markers
data Coin = Coin deriving Component
data Hero = Hero deriving Component
