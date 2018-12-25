module Examples.Orrery.Planet where

import Control.Lens
import Graphics.Gloss

data Planet = Planet
  { _hue :: Color
  , _radius :: Float
  , _year :: Float
  , _orbitDistance :: Float
  }

makeLenses ''Planet
