module Examples.Fountain.Fountain where

import Control.Lens

import ReduxGame.Entities.Entities
import ReduxGame.Entities.Entity
import ReduxGame.Entities.EntityRedux
import ReduxGame.Shape.Shape
import ReduxGame.Redux
import ReduxGame.Timer
import ReduxGame.Renderer.Renderable
import ReduxGame.Renderer.ShapeRenderer
import Graphics.Gloss hiding (circle)

type World = ComponentStore ListStore

data Position = Position Float Float deriving Component
data Velocity = Velocity Float Float deriving Component

instance Component Shape
instance Component Color

data Fountain = Fountain
  { _world :: World
  , _timer :: Timer
  }

makeLenses ''Fountain

fountain :: Fountain
fountain = Fountain emptyComponents newTimer

instance Renderable Fountain where
  render fountain = Pictures $ foldStore render' (fountain ^. world) where
    render' :: (Position, Shape, Color) -> Picture
    render' ((Position x y), s, c) = translate x y $ color c $ render s

initialiseFountain :: Events ()
initialiseFountain = schedule 2 createShiny

createShiny :: Events ()
createShiny = spawn $ entity <-+ Position 0 0 <-+ Velocity 400 0 <-+ yellow <-+ circle 0 20

integrate :: TimeStep -> (Position, Velocity) -> Only Position
integrate (TimeStep t) ((Position x y), (Velocity dx dy)) = Only $ Position (x + dx * t) (y + dy * t)

worldRedux :: Redux World
worldRedux = entityRedux
         |$> integrate

fountainRedux :: Redux Fountain
fountainRedux = redux
            |:: connect world worldRedux
            |:: connect timer timerRedux
