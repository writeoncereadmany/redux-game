module Examples.Pandamonium.Pandamonium where

import Control.Lens

import ReduxGame.Redux
import ReduxGame.Exit
import ReduxGame.Entities
import ReduxGame.Components
import ReduxGame.Collisions
import ReduxGame.Renderer.Renderable
import ReduxGame.WorldShapeRenderer

import Examples.Pandamonium.Labels

import Examples.Pandamonium.Controllers.Pickups
import Examples.Pandamonium.Controllers.HeroMovement
import Examples.Pandamonium.Controllers.Controls
import Examples.Pandamonium.Controllers.Physics

import Examples.Pandamonium.Stages.Stage1
import Examples.Pandamonium.Stages.Stage2

data PandaGame = PandaGame
  { _world :: World
  , _stages :: [ [Entity] ]
  }

makeLenses ''PandaGame

instance Renderable PandaGame where
  render pg = render $ pg ^. world

initialPandas = PandaGame newWorld [stage1, stage2]

countCoins :: World -> Int
countCoins world = actuallyFold count 0 world where
  count :: Only Coin -> Int -> Int
  count _ x = x + 1

checkForCompletion :: TimeStep -> PandaGame -> Events PandaGame
checkForCompletion _ pg =
  if countCoins (pg ^. world) == 0
    then case (pg ^. stages) of
      [] -> do
        quit
        return pg
      (next : rest) -> do
        traverse spawn next
        return $ PandaGame newWorld rest
    else return pg

pandaWorldRedux :: Redux World
pandaWorldRedux = worldRedux
              |:: pickupRedux
              |:: heroRedux
              |:: controlRedux
              |:: physicsRedux

pandaGameRedux :: Redux PandaGame
pandaGameRedux = redux
             |:: connect world pandaWorldRedux
             |=> checkForCompletion
