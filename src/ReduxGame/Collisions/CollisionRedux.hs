module ReduxGame.Collisions.CollisionRedux
  ( collisionRedux
  , fireOnCollision
  ) where

import Control.Monad
import Control.Monad.Trans
import Graphics.Gloss.Data.Vector

import ReduxGame.Entities
import ReduxGame.Entities.Store.Interactions
import ReduxGame.Components.Components
import ReduxGame.Shape.Shape
import ReduxGame.Redux
import ReduxGame.Collisions.CollisionEvents
import ReduxGame.Collisions.CollisionDetection
import ReduxGame.Collisions.CollisionReaction


detectStaticCollisions :: Tagged StaticObject -> Tagged MovingObject -> Events ()
detectStaticCollisions (Tagged s_id (StaticObject _ s_shp (Position s_pos))) (Tagged m_id (MovingObject _ m_shp (Position m_pos) _)) =
  when (move s_pos s_shp !!! move m_pos m_shp) (fireEvent $ StaticCollision s_id m_id)

detectMovingCollisions :: Tagged MovingObject -> Tagged MovingObject -> Events ()
detectMovingCollisions (Tagged a_id (MovingObject _ a_shp (Position a_pos) _)) (Tagged b_id (MovingObject _ b_shp (Position b_pos) _)) = do
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

detectCollisions :: TimeStep -> World -> Events World
detectCollisions _ = relate detectStaticCollisions
                 >=> selfRelate2 detectMovingCollisions naiveBroadphase

collisionRedux :: Redux World
collisionRedux = redux
             |=> detectCollisions
             |=> staticBounce
             |=> movingBounce
