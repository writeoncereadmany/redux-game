module ReduxGame.Collisions.CollisionEvents where

import Graphics.Gloss.Interface.IO.Game

import ReduxGame.Redux
import ReduxGame.Entities

newtype Static = Static Float deriving Component
data Moving = Moving Float Float deriving Component

data StaticCollision = StaticCollision EntityId EntityId deriving ReduxEvent
data MovingCollision = MovingCollision EntityId EntityId deriving ReduxEvent

data Pushed = Pushed EntityId Vector deriving ReduxEvent
