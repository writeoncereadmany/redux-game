module ReduxGame.Collisions.CollisionEvents where

import ReduxGame.Redux
import ReduxGame.Entities
import ReduxGame.Components.Components
import ReduxGame.Shape.Shape

data Static = Static Float deriving Component
data Moving = Moving Float Float deriving Component

data StaticCollision = StaticCollision EntityId EntityId deriving ReduxEvent
data MovingCollision = MovingCollision EntityId EntityId deriving ReduxEvent

data StaticObject = StaticObject Static Shape Position
data MovingObject = MovingObject Moving Shape Position Velocity


instance Extractable StaticObject where
  extract = extract_2r1d StaticObject
  extractWithId = extractWithId_2r1d StaticObject

instance Extractable MovingObject where
  extract = extract_2r2d MovingObject
  extractWithId = extractWithId_2r2d MovingObject
