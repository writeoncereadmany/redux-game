module ReduxGame.Collisions.CollisionEvents where

import ReduxGame.Redux
import ReduxGame.Entities.Entity

data Elasticity = Elasticity Float deriving Component
instance Default Elasticity where defaultValue = Elasticity 0

data Mass = Mass Float deriving Component

data Static = Static Float deriving Component
data Moving = Moving deriving Component

data StaticCollision = StaticCollision EntityId EntityId deriving ReduxEvent
data MovingCollision = MovingCollision EntityId EntityId deriving ReduxEvent
