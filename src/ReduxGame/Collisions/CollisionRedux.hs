module ReduxGame.Collisions.CollisionRedux where

import Control.Monad
import Graphics.Gloss (Vector)

import ReduxGame.Entities.Store.Interactions
import ReduxGame.Entities.Store.ComponentStore
import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Entity
import ReduxGame.Shape.Shape
import ReduxGame.Redux
import ReduxGame.Collisions.Collisions


data Static = Static deriving Component
data Moving = Moving deriving Component

data Position = Position Vector deriving Component

instance Component Shape

data StaticCollision = StaticCollision EntityId EntityId deriving ReduxEvent
data MovingCollision = MovingCollision EntityId EntityId deriving ReduxEvent

detectStaticCollisions :: Tagged (Static, Position, Shape) -> Tagged (Moving, Position, Shape) -> Events ()
detectStaticCollisions (Tagged s_id (_, (Position s_pos), s_shp)) (Tagged m_id (_, (Position m_pos), m_shp)) = do
  let s_shp' = move s_pos s_shp
  let m_shp' = move m_pos m_shp
  when (s_shp' !!! m_shp') (fireEvent $ StaticCollision s_id m_id)

bounce :: Tagged (Moving, Position, Shape) -> Tagged (Moving, Position, Shape) -> Events ()
bounce (Tagged a_id (_, (Position a_pos), a_shp)) (Tagged b_id (_, (Position b_pos), b_shp)) = do
  let a_shp' = move a_pos a_shp
  let b_shp' = move b_pos b_shp
  when (a_shp' !!! b_shp') (fireEvent $ MovingCollision a_id b_id)

detectCollisions :: Store s => TimeStep -> ComponentStore s -> Events (ComponentStore s)
detectCollisions _ cs = return cs
                    >>= relate detectStaticCollisions
                    >>= selfRelate bounce
