module Examples.Pandamonium.Entities.Hero where

import Control.Lens

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Shape
import ReduxGame.Components
import ReduxGame.Collisions
import ReduxGame.Controls
import ReduxGame.WorldShapeRenderer

import Examples.Pandamonium.Assets.PandaAssets
import Examples.Pandamonium.Labels
import Examples.Pandamonium.Events

w = 120
h = 120

gravity = -2400

data GroundedState = Grounded | Airborne deriving (Eq, Component)

data PandaFrames = PandaFrames
  { _run_animation :: [Picture]
  , _standing :: Picture
  , _jumping :: Picture
  , _falling :: Picture
  , _sliding :: Picture
  } deriving Component

makeLenses ''PandaFrames

frame :: BitmapData -> Int -> Picture
frame bmp n = scale 4 4 (BitmapSection (Rectangle (n*25, 0) (25, 28)) bmp)

loadFrames :: PandaAssets -> PandaFrames
loadFrames assets = let frame' = frame $ assets ^. panda_sprites
  in PandaFrames
    { _run_animation = [ frame' n | n <- [0,1,2,1]]
    , _standing = frame' 1
    , _jumping = frame' 0
    , _falling = frame' 0
    , _sliding = frame' 1
    }

data Facing = FacingLeft | FacingRight deriving Component

hero :: PandaAssets -> Vector -> Entity
hero assets position = entity
            <-+ Hero
            <-+ Camera

            <-+ rectangle (-w/2, -h/2) (w, h)
            <-+ scale 6 6 (BitmapSection (Rectangle (140, 0) (20, 20)) (assets ^. panda_sprites))
            <-+ loadFrames assets

            <-+ Position position
            <-+ Velocity (0, 0)
            <-+ Acceleration (0, gravity)

            <-+ AxisType Horizontal (axis (button 'z') (button 'x'))
            <-+ ButtonType Jump ((button '.') { onPress = fireEvent JumpEvent })

            <-+ Moving 0 1
            <-+ FeelsGravity
            <-+ Airborne
            <-+ FacingRight
