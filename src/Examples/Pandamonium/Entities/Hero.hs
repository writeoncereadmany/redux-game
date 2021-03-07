module Examples.Pandamonium.Entities.Hero where

import Control.Lens

import Graphics.Gloss

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Shape
import ReduxGame.Components
import ReduxGame.Collisions
import ReduxGame.Controls

import Examples.Pandamonium.Assets.PandaAssets
    ( panda_sprites, PandaAssets )
import Examples.Pandamonium.Labels
import Examples.Pandamonium.Events

sprite_width = 18
sprite_height = 22

w = sprite_width
h = sprite_height

gravity = -300

data GroundedState = Grounded | Ascending | Falling deriving (Eq, Component)

data PandaFrames = PandaFrames
  { _run_animation :: [Picture]
  , _standing :: Picture
  , _jumping :: Picture
  , _floating :: Picture
  , _falling :: Picture
  } deriving Component

makeLenses ''PandaFrames

loadFrames :: PandaAssets -> PandaFrames
loadFrames assets = let frame n = (Bitmap <$> (assets ^. panda_sprites)) !! n
  in PandaFrames
    { _run_animation = [ frame n | n <- [0,1,2,3,2,1]]
    , _standing = frame 4
    , _jumping = frame 5
    , _floating = frame 6
    , _falling = frame 7
    }

data Facing = FacingLeft | FacingRight deriving Component

hero :: PandaAssets -> Vector -> Entity
hero assets position = entity
            <-+ Hero

            <-+ rectangle (-w/2, -h/2) (w, h)
            <-+ loadFrames assets

            <-+ Position position
            <-+ Velocity 0
            <-+ Acceleration 0

            <-+ AxisType Horizontal (axis (button 'z') (button 'x'))
            <-+ ButtonType Jump ((button '.') { onPress = fireEvent JumpEvent })

            <-+ Moving 0 1
            <-+ FeelsGravity
            <-+ Falling
            <-+ FacingRight
