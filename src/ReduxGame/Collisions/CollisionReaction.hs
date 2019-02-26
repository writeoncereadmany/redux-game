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

data StaticObject = StaticObject Static Shape Position
data MovingObject = MovingObject Moving Shape Position Velocity

data AfterCollision = AfterCollision Position Velocity

collisionToTuple (AfterCollision pos vel) = (pos, vel)

instance Extractable StaticObject where
  extract = extract_2r1d StaticObject
  extractWithId = extractWithId_2r1d StaticObject

instance Extractable MovingObject where
  extract = extract_2r2d MovingObject
  extractWithId = extractWithId_2r2d MovingObject

instance Persistable AfterCollision where
  persist as = persist $ fmap collisionToTuple <$> as

staticBounce' :: StaticObject -> MovingObject -> AfterCollision
staticBounce' (StaticObject (Static s_el) s_shp (Position s_pos))
              (MovingObject _ m_shp (Position m_pos) (Velocity m_vel))
  = case (move s_pos s_shp !!> move m_pos m_shp) of
    Nothing -> AfterCollision (Position m_pos) (Velocity m_vel)
    (Just pushout) -> let
      elasticity  = 1 + s_el
      m_pos'      = mulSV elasticity pushout + m_pos
      unit_push   = normalizeV pushout
      normal_proj = elasticity * (m_vel `dotV` unit_push)
      m_vel'      = m_vel + (negate $ mulSV normal_proj unit_push)
      in AfterCollision (Position m_pos') (Velocity m_vel')

staticBounce :: Store s => StaticCollision -> ComponentStore s -> Events (ComponentStore s)
staticBounce (StaticCollision s_id m_id) cs = do
  let new_pos_and_vel = pure staticBounce' <*> extractWithId s_id cs <*> extractWithId m_id cs
  return $ maybe cs (flip (persistWithId m_id) cs) new_pos_and_vel
