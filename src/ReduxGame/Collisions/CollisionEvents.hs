module ReduxGame.Collisions.CollisionEvents where

import ReduxGame.Redux
import ReduxGame.Entities.Entity

data Static = Static Float deriving Component
data Moving = Moving Float Float deriving Component

data StaticCollision = StaticCollision EntityId EntityId deriving ReduxEvent
data MovingCollision = MovingCollision EntityId EntityId deriving ReduxEvent
