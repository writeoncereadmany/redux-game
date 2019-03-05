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
import ReduxGame.Collisions.CollisionEvents
import ReduxGame.Collisions.CollisionDetection
import ReduxGame.Collisions.CollisionReaction


detectStaticCollisions :: Tagged (Static, Position, Shape) -> Tagged (Moving, Velocity, Position, Shape) -> Events ()
detectStaticCollisions (Tagged s_id (_, (Position s_pos), s_shp)) (Tagged m_id (_, _, (Position m_pos), m_shp)) =
  when (move s_pos s_shp !!! move m_pos m_shp) (fireEvent $ StaticCollision s_id m_id)

detectMovingCollisions :: Tagged (Moving, Position, Shape) -> Tagged (Moving, Position, Shape) -> Events ()
detectMovingCollisions (Tagged a_id (_, (Position a_pos), a_shp)) (Tagged b_id (_, (Position b_pos), b_shp)) = do
  when (move a_pos a_shp !!! move b_pos b_shp) (fireEvent $ MovingCollision a_id b_id)

detectCollisions :: Store s => TimeStep -> ComponentStore s -> Events (ComponentStore s)
detectCollisions _ cs = return cs
                    >>= relate detectStaticCollisions
                    >>= selfRelate2 detectMovingCollisions naiveBroadphase

collisionRedux :: Store s => Redux (ComponentStore s)
collisionRedux = redux
             |=> detectCollisions
             |=> staticBounce
             |=> movingBounce
