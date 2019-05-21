module Examples.Pandamonium.Controllers.Pickups where

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Collisions

import Examples.Pandamonium.Labels

collectCoins :: TimeStep -> World -> Events World
collectCoins = fireOnCollision Hero Coin destroyCoin where
  destroyCoin hero_id coin_id = destroy coin_id


pickupRedux :: Redux World
pickupRedux = redux
          |=> collectCoins
