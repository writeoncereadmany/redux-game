module ReduxGame.Collisions.CollisionReaction
  ( staticBounce
  , movingBounce
  ) where

import Data.Maybe

import Graphics.Gloss.Data.Vector

import ReduxGame.Entities
import ReduxGame.Components.Components
import ReduxGame.Shape
import ReduxGame.Redux

import ReduxGame.Collisions.CollisionDetection
import ReduxGame.Collisions.CollisionEvents

data AfterCollision = AfterCollision Position Velocity

collisionToTuple (AfterCollision pos vel) = (pos, vel)

instance Persistable AfterCollision where
  persist as = persist $ fmap collisionToTuple <$> as

staticBounce' :: EntityId -> StaticObject -> EntityId -> MovingObject -> Events AfterCollision
staticBounce' s_id (StaticObject (Static s_el) s_shp (Position s_pos))
              m_id (MovingObject (Moving m_el _) m_shp (Position m_pos) (Velocity m_vel))
  = case (move s_pos s_shp !!> move m_pos m_shp) of
    Nothing -> return $ AfterCollision (Position m_pos) (Velocity m_vel)
    (Just pushout) -> do
      let elasticity  = s_el * m_el
      let pushout'    = mulSV (1 + elasticity) pushout
      let m_pos'      = m_pos + pushout'
      let unit_push   = normalizeV pushout
      let normal_proj = (1 + elasticity) * (m_vel `dotV` unit_push)
      let m_vel'      = if (normal_proj > 0) then m_vel else m_vel + (negate $ mulSV normal_proj unit_push)
      fireEvent $ Pushed m_id pushout'
      return $ AfterCollision (Position m_pos') (Velocity m_vel')

staticBounce :: StaticCollision -> World -> Events World
staticBounce (StaticCollision s_id m_id) cs = do
  case (extractWithId s_id cs, extractWithId m_id cs) of
    (Just static, Just moving) -> do
      moving' <- staticBounce' s_id static m_id moving
      return $ persistWithId m_id moving' cs
    otherwise -> return cs

movingBounce' :: EntityId -> MovingObject -> EntityId -> MovingObject -> Events (AfterCollision, AfterCollision)
movingBounce' a_id (MovingObject (Moving ae am) as (Position ap) (Velocity av))
              b_id (MovingObject (Moving be bm) bs (Position bp) (Velocity bv)) =
  case (move ap as !!> move bp bs) of
    Nothing -> return (AfterCollision (Position ap) (Velocity av), AfterCollision (Position bp) (Velocity bv))
    (Just pushout) -> do
      let elasticity          = ae * be
      let totalMass           = am + bm
      let a_mass_share        = am / totalMass
      let b_mass_share        = bm / totalMass
      let zero_momentum_frame = mulSV a_mass_share av + mulSV b_mass_share bv
      let rel_av              = av - zero_momentum_frame
      let rel_bv              = bv - zero_momentum_frame
      let pushout_a           = mulSV a_mass_share (negate pushout)
      let pushout_b           = mulSV b_mass_share pushout
      let ap'                 = ap + pushout_a
      let bp'                 = bp + pushout_b
      let unit_push           = normalizeV pushout
      let dav                 = negate $ mulSV ((1 + elasticity) * (rel_av `dotV` unit_push)) unit_push
      let dbv                 = negate $ mulSV ((1 + elasticity) * (rel_bv `dotV` unit_push)) unit_push
      fireEvent $ Pushed a_id pushout_a
      fireEvent $ Pushed b_id pushout_b
      return (AfterCollision (Position ap') (Velocity (av + dav)), AfterCollision (Position bp') (Velocity (bv + dbv)))

movingBounce :: MovingCollision -> World -> Events World
movingBounce (MovingCollision a_id b_id) cs = do
  case (extractWithId a_id cs, extractWithId b_id cs) of
    (Just as, Just bs) -> do
      (as', bs') <- movingBounce' a_id as b_id bs
      return $ persistWithId a_id as' $ persistWithId b_id bs' $ cs
    otherwise -> return cs
