module Examples.Pandamonium.Controllers.CoinAnimation where

import ReduxGame.Shape
import Examples.Pandamonium.Events
import Examples.Pandamonium.Entities.Coin

animateCoin :: Pulse -> (Shape, AnimationFrames) -> (Shape, AnimationFrames)
animateCoin _ (current, AnimationFrames []) = (current, AnimationFrames [])
animateCoin _ (current, AnimationFrames (next:rest)) = (next, AnimationFrames rest)
