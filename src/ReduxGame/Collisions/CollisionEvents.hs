module ReduxGame.Collisions.CollisionEvents where

import ReduxGame.Redux
import ReduxGame.Entities.Entity

data StaticCollision = StaticCollision EntityId EntityId deriving ReduxEvent
data MovingCollision = MovingCollision EntityId EntityId deriving ReduxEvent
