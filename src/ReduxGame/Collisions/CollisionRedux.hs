module ReduxGame.Collisions.CollisionRedux
  ( collisionRedux
  , fireOnCollision
  , fireOnCollision'
  ) where

import Control.Monad

import ReduxGame.Entities
import ReduxGame.Entities.Interactions
import ReduxGame.Components.Components
import ReduxGame.Shape
import ReduxGame.Redux
import ReduxGame.Collisions.CollisionEvents
import ReduxGame.Collisions.CollisionDetection
import ReduxGame.Collisions.CollisionReaction


detectStaticCollisions :: Tagged (Static, Shape, Maybe Position)
                       -> Tagged (Moving, Shape, Maybe Position, Maybe Velocity)
                       -> Events ()
detectStaticCollisions (Tagged s_id (_, s_shp, maybe_sp)) (Tagged m_id (_, m_shp, maybe_mp, _)) = do
  let s_pos = maybe (0, 0) unwrap maybe_sp
  let m_pos = maybe (0, 0) unwrap maybe_mp
  when (move s_pos s_shp !!! move m_pos m_shp) (fireEvent $ StaticCollision s_id m_id)

detectMovingCollisions :: Tagged (Moving, Shape, Maybe Position, Maybe Velocity)
                       -> Tagged (Moving, Shape, Maybe Position, Maybe Velocity)
                       -> Events ()
detectMovingCollisions (Tagged a_id (_, a_shp, maybe_ap, _)) (Tagged b_id (_, b_shp, maybe_bp, _)) = do
  let a_pos = maybe (0,0) unwrap maybe_ap
  let b_pos = maybe (0,0) unwrap maybe_bp
  when (move a_pos a_shp !!! move b_pos b_shp) (fireEvent $ MovingCollision a_id b_id)

detectEventCollisions :: forall a b . (Component a, Component b)
                      => a -> b -> (EntityId -> EntityId -> Events ())
                      -> Tagged (a, Shape, Position) -> Tagged (b, Shape, Position) -> Events ()
detectEventCollisions _ _ event (Tagged a_id (_, a_shp, Position a_pos)) (Tagged b_id (_, b_shp, Position b_pos)) =
  when (move a_pos a_shp !!! move b_pos b_shp) (event a_id b_id)

fireOnCollision :: forall a b . (Component a, Component b)
                => a -> b -> (EntityId -> EntityId -> Events ())
                -> TimeStep -> World -> Events World
fireOnCollision a b f _ = relate $ detectEventCollisions a b f

fireOnCollision' :: forall a b e . (Component a, Component b, ReduxEvent e)
                 => a -> b -> (EntityId -> EntityId -> e)
                 -> TimeStep -> World -> Events World
fireOnCollision' a b f = fireOnCollision a b (\c d -> fireEvent $ f c d)

detectCollisions :: TimeStep -> World -> Events World
detectCollisions _ = relate detectStaticCollisions
                 >=> selfRelate detectMovingCollisions naiveBroadphase

collisionRedux :: Redux World
collisionRedux = redux
             |=> detectCollisions
             |=> staticBounce
             |=> movingBounce
