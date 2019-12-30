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

staticBounce' :: EntityId
              -> (Static, Shape, Maybe Position)
              -> EntityId
              -> (Moving, Shape, Maybe Position, Maybe Velocity)
              -> Events (Position, Velocity)
staticBounce' s_id ((Static s_el), s_shp, maybe_s_pos)
              m_id ((Moving m_el _), m_shp, maybe_m_pos, maybe_m_vel)
  = let s_pos = maybe 0 unwrap maybe_s_pos
        m_pos = maybe 0 unwrap maybe_m_pos
        m_vel = maybe 0 unwrap maybe_m_vel
    in case (move s_pos s_shp !!> move m_pos m_shp) of
    Nothing -> return $ (Position m_pos, Velocity m_vel)
    (Just pushout) -> do
      let elasticity  = s_el * m_el
      let pushout'    = mulSV (1 + elasticity) pushout
      let m_pos'      = m_pos + pushout'
      let unit_push   = normalizeV pushout
      let normal_proj = (1 + elasticity) * (m_vel `dotV` unit_push)
      let m_vel'      = if (normal_proj > 0) then m_vel else m_vel - (mulSV normal_proj unit_push)
      fireEvent $ Pushed m_id pushout'
      return (Position m_pos', Velocity m_vel')

staticBounce :: StaticCollision -> World -> Events World
staticBounce (StaticCollision s_id m_id) cs = do
  case (getById s_id cs, getById m_id cs) of
    (Just static, Just moving) -> do
      moving' <- staticBounce' s_id static m_id moving
      return $ setAll [ Tagged m_id moving' ] cs
    otherwise -> return cs

movingBounce' :: EntityId
              -> (Moving, Shape, Maybe Position, Maybe Velocity)
              -> EntityId
              -> (Moving, Shape, Maybe Position, Maybe Velocity)
              -> Events ((Position, Velocity), (Position, Velocity))
movingBounce' a_id ((Moving ae am), as, maybe_ap, maybe_av)
              b_id ((Moving be bm), bs, maybe_bp, maybe_bv) =
  let ap = maybe 0 unwrap maybe_ap
      av = maybe 0 unwrap maybe_av
      bp = maybe 0 unwrap maybe_bp
      bv = maybe 0 unwrap maybe_bv
  in case (move ap as !!> move bp bs) of
    Nothing -> return ((Position ap, Velocity av), (Position bp, Velocity bv))
    (Just pushout) -> do
      let elasticity          = ae * be
      let a_mass_share        = am / (am + bm)
      let b_mass_share        = bm / (am + bm)
      let zero_momentum_frame = mulSV a_mass_share av + mulSV b_mass_share bv
      let rel_av              = av - zero_momentum_frame
      let rel_bv              = bv - zero_momentum_frame
      let pushout_a           = mulSV a_mass_share (-pushout)
      let pushout_b           = mulSV b_mass_share pushout
      let ap'                 = ap + pushout_a
      let bp'                 = bp + pushout_b
      let unit_push           = normalizeV pushout
      let dav                 = mulSV ((1 + elasticity) * (rel_av `dotV` unit_push)) unit_push
      let dbv                 = mulSV ((1 + elasticity) * (rel_bv `dotV` unit_push)) unit_push
      fireEvent $ Pushed a_id pushout_a
      fireEvent $ Pushed b_id pushout_b
      return ((Position ap', Velocity (av - dav)), (Position bp', Velocity (bv - dbv)))

movingBounce :: MovingCollision -> World -> Events World
movingBounce (MovingCollision a_id b_id) cs = do
  case (getById a_id cs, getById b_id cs) of
    (Just as, Just bs) -> do
      (as', bs') <- movingBounce' a_id as b_id bs
      return $ setAll [ Tagged a_id as' ] $ setAll [Tagged b_id bs' ] $ cs
    otherwise -> return cs
