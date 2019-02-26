module ReduxGame.Collisions.CollisionReaction where

import Data.Maybe

import Graphics.Gloss.Data.Vector

import ReduxGame.Entities.Entity
import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Store.ComponentStore
import ReduxGame.Components.Components
import ReduxGame.Entities.Store.Variadics
import ReduxGame.Shape.Shape
import ReduxGame.Redux

import ReduxGame.Collisions.CollisionDetection
import ReduxGame.Collisions.CollisionEvents

data StaticObject = StaticObject Shape Elasticity Position
data MovingObject = MovingObject Shape Elasticity Mass Position Velocity

data AfterCollision = AfterCollision Position Velocity

collisionToTuple (AfterCollision pos vel) = (pos, vel)

instance Extractable StaticObject where
  extract cs = let shapes = storeOf cs
                in buildStaticObject <$> shapes
             where buildStaticObject :: Tagged Shape -> Tagged StaticObject
                   buildStaticObject (Tagged entId shp) =
                     let position = getComponent entId cs
                         elasticity = getComponent entId cs
                      in Tagged entId (StaticObject shp elasticity position)
  extractWithId entId cs = do
    shape <- maybeGetComponent entId cs
    let position = getComponent entId cs
    let elasticity = getComponent entId cs
    return $ StaticObject shape position elasticity

-- instance Extractable MovingObject

instance Persistable AfterCollision where
  persist as = persist $ fmap collisionToTuple <$> as

staticBounce' :: StaticObject -> (Position, Velocity, Shape) -> AfterCollision
staticBounce' (StaticObject s_shp _ (Position s_pos)) ((Position m_pos), (Velocity m_vel), m_shp) = case (move s_pos s_shp !!> move m_pos m_shp) of
  Nothing -> AfterCollision (Position m_pos) (Velocity m_vel)
  (Just pushout) -> let
    m_pos'      = mulSV 2 pushout + m_pos
    unit_push   = normalizeV pushout
    normal_proj = 2 * (m_vel `dotV` unit_push)
    m_vel'      = m_vel + (negate $ mulSV normal_proj unit_push)
    in AfterCollision (Position m_pos') (Velocity m_vel')

staticBounce :: Store s => StaticCollision -> ComponentStore s -> Events (ComponentStore s)
staticBounce (StaticCollision s_id m_id) cs = do
  let new_pos_and_vel = pure staticBounce' <*> extractWithId s_id cs <*> extractWithId m_id cs
  return $ maybe cs (flip (persistWithId m_id) cs) new_pos_and_vel
