module Examples.Pandamonium.Controllers.CoinAnimation where

import Graphics.Gloss
import Examples.Pandamonium.Events
import Examples.Pandamonium.Entities.Coin

animateCoin :: Pulse -> (Picture, AnimationFrames) -> (Picture, AnimationFrames)
animateCoin _ (current, AnimationFrames []) = (current, AnimationFrames [])
animateCoin _ (current, AnimationFrames (next:rest)) = (next, AnimationFrames rest)
