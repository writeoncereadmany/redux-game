module Examples.Orrery.Orrery where

import Graphics.Gloss (color, yellow, blue, makeColor)
import Graphics.Gloss (circleSolid, Picture (Pictures, Blank))
import Control.Lens

import ReduxGame.Redux
import ReduxGame.Timer
import ReduxGame.Shape.Shape
import ReduxGame.Renderer.ShapeRenderer
import ReduxGame.Renderer.Renderable

import Examples.Orrery.Planet

data Orrery = Orrery
  { _timer :: Timer
  , _planets :: [ Planet ]
  }

makeLenses ''Orrery

orrery :: Orrery
orrery = Orrery newTimer
  [ Planet { _orbitDistance = 300, _year = 10, _hue = makeColor 0.6 0.8 1 1, _radius = 20 }
  , Planet { _orbitDistance = 250, _year = 12, _hue = makeColor 0.7 1 0.5 1, _radius = 15 }
  , Planet { _orbitDistance = 200, _year = 7,  _hue = makeColor 1 1 0.4 1,   _radius = 8}
  , Planet { _orbitDistance = 450, _year = 13, _hue = makeColor 1 0.4 0.5 1, _radius = 18 }
  ]

drawPlanet :: Float -> Planet -> Picture
drawPlanet elapsed planet =
  let planetCircle = circle (0, 0) (planet ^. radius)
      season = (elapsed / planet ^. year) * (2 * pi)
      x = (planet ^. orbitDistance) * sin season
      y = (planet ^. orbitDistance) * cos season
      planetCircle' = move (x, y) planetCircle
   in color (planet ^. hue) (render planetCircle')

instance Renderable Orrery where
  render orrery =
    let sunPic = color yellow $ circleSolid 50
        planetPics = drawPlanet (orrery ^. timer . elapsed) <$> (orrery ^. planets)
     in Pictures (sunPic : planetPics)

orreryRedux :: Redux Orrery
orreryRedux = redux
          |:: connect timer timerRedux
