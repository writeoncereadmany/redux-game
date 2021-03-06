module Examples.Pandamonium.Pandamonium where

import Control.Lens
import Control.Monad

import Graphics.Gloss

import ReduxGame.Redux
import ReduxGame.Timer
import ReduxGame.Exit
import ReduxGame.Entities
import ReduxGame.Renderer.Renderable
import ReduxGame.WorldShapeRenderer ()

import Examples.Pandamonium.Labels
import Examples.Pandamonium.Events

import Examples.Pandamonium.Assets.PandaAssets

import Examples.Pandamonium.Controllers.Pickups
import Examples.Pandamonium.Controllers.HeroMovement
import Examples.Pandamonium.Controllers.HeroAnimation
import Examples.Pandamonium.Controllers.Controls
import Examples.Pandamonium.Controllers.Physics
import Examples.Pandamonium.Controllers.CoinAnimation

import Examples.Pandamonium.Stages.Stage1
import Examples.Pandamonium.Stages.Stage2

data PandaGame = PandaGame
  { _world :: World
  , _stages :: [ [Entity] ]
  , _timeLeft :: Integer
  , _timer :: Timer
  }

makeLenses ''PandaGame

instance Renderable PandaGame where
  render pg = Pictures
    [ translate 400 150 $ scale 0.2 0.2 $ color white $ text (show $ pg ^. timeLeft)
    , scale 4.5 4.5 $ render (pg ^. world)
    ]

initialPandas :: PandaAssets -> PandaGame
initialPandas assets = PandaGame newWorld (take 5 $ cycle [stage1 assets, stage2 assets]) 10 newTimer

initialisePandas :: Events ()
initialisePandas = schedule 0.15 (fireEvent Pulse)

countCoins :: World -> Int
countCoins = foldStore count 0 where
  count :: Coin -> Int -> Int
  count _ x = x + 1

checkForCompletion :: TimeStep -> PandaGame -> Events PandaGame
checkForCompletion _ pg = do
  when (countCoins (pg ^. world) == 0) (fireEvent LevelComplete)
  return pg

nextLevel :: LevelComplete -> PandaGame -> Events PandaGame
nextLevel _ pg@(PandaGame _ [] _ timer) = do
  fireEvent GameOver
  return pg
nextLevel _ (PandaGame _ (next:rest) _ timer) = do
  traverse spawn next
  return $ PandaGame newWorld rest 100 timer

exitOnGameOver :: GameOver -> PandaGame -> Events PandaGame
exitOnGameOver _ pg = do
  quit
  return pg

pandaWorldRedux :: Redux World
pandaWorldRedux = worldRedux
              |:: pickupRedux
              |:: heroMovementRedux
              |:: heroAnimationRedux
              |:: controlRedux
              |:: physicsRedux
              |$> animateCoin

pandaGameRedux :: Redux PandaGame
pandaGameRedux = redux
             |:: connect world pandaWorldRedux
             |=> checkForCompletion
             |=> nextLevel
             |=> exitOnGameOver
             |:: connect timer timerRedux
