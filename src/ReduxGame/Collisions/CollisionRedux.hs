module ReduxGame.Collisions.CollisionRedux where

import Control.Monad
import Control.Monad.Trans
import Graphics.Gloss.Data.Vector

import ReduxGame.Entities.Store.Interactions
import ReduxGame.Entities.Store.ComponentStore
import ReduxGame.Entities.Store.Variadics
import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Entity
import ReduxGame.Components.Components
import ReduxGame.Shape.Shape
import ReduxGame.Redux
import ReduxGame.Collisions.CollisionDetection

data StaticCollision = StaticCollision EntityId EntityId deriving ReduxEvent
data MovingCollision = MovingCollision EntityId EntityId deriving ReduxEvent

detectStaticCollisions :: Tagged (Static, Position, Shape) -> Tagged (Moving, Velocity, Position, Shape) -> Events ()
detectStaticCollisions (Tagged s_id (_, (Position s_pos), s_shp)) (Tagged m_id (_, _, (Position m_pos), m_shp)) =
  when (move s_pos s_shp !!! move m_pos m_shp) (fireEvent $ StaticCollision s_id m_id)

detectMovingCollisions :: Tagged (Moving, Position, Shape) -> Tagged (Moving, Position, Shape) -> Events ()
detectMovingCollisions (Tagged a_id (_, (Position a_pos), a_shp)) (Tagged b_id (_, (Position b_pos), b_shp)) = do
  when (move a_pos a_shp !!! move b_pos b_shp) (fireEvent $ MovingCollision a_id b_id)

detectCollisions :: Store s => TimeStep -> ComponentStore s -> Events (ComponentStore s)
detectCollisions _ cs = return cs
                    >>= relate detectStaticCollisions
                    >>= selfRelate detectMovingCollisions

-- it's possible that since the event was fired, other collision events etc have
-- already moved one of the objects so they no longer collide
-- so we re-check the collision before determining the new position/velocity
staticBounce :: Store s => StaticCollision -> ComponentStore s -> Events (ComponentStore s)
staticBounce (StaticCollision s_id m_id) cs = do
  let new_pos_and_vel = pure staticBounce' <*> extractWithId s_id cs <*> extractWithId m_id cs
  let update = (\a -> [Tagged m_id a]) <$> new_pos_and_vel
  return $ maybe cs (flip persist cs) update where
    staticBounce' :: (Position, Shape) -> (Position, Velocity, Shape) -> (Position, Velocity)
    staticBounce' ((Position s_pos), s_shp) ((Position m_pos), (Velocity m_vel), m_shp) = case (move s_pos s_shp !!> move m_pos m_shp) of
      Nothing -> (Position m_pos, Velocity m_vel)
      (Just pushout) -> let
        m_pos'      = mulSV 2 pushout + m_pos
        unit_push   = normalizeV pushout
        normal_proj = 2 * (m_vel `dotV` unit_push)
        m_vel'      = m_vel + (negate $ mulSV normal_proj unit_push)
        in (Position m_pos', Velocity m_vel')

collisionRedux :: Store s => Redux (ComponentStore s)
collisionRedux = redux
             |=> detectCollisions
             |=> staticBounce
