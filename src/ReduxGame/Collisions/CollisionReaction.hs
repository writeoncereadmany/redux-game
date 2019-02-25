module ReduxGame.Collisions.CollisionReaction where

import ReduxGame.Entities.Entity
import ReduxGame.Components.Components
import ReduxGame.Entities.Store.Variadics
import ReduxGame.Shape.Shape

data Elasticity = Elasticity Float deriving Component
data Mass = Mass Float deriving Component

data StaticObject = StaticObject Shape Elasticity Position
data MovingObject = MovingObject Shape Elasticity Mass Position Velocity

data AfterCollision = AfterCollision Position Velocity

staticObject (s, e, p) = StaticObject s e p

collisionToTuple (AfterCollision pos vel) = (pos, vel)

instance Extractable StaticObject where
  extract = fmap (fmap staticObject) . extract
  extractWithId entId = fmap staticObject . extractWithId entId

instance Persistable AfterCollision where
  persist as = persist $ fmap collisionToTuple <$> as
