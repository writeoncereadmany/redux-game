module ReduxGame.Entities
  -- entity definitions
  ( Component (getAll, getById, setAll)
  , Property
  , EntityId
  , Entity
  , entity
  , (<-+)
  , (<++)
  , Tagged (Tagged)
  , Not (Not)
  -- entity redux combinators
  , (|$>)
  , (|*>)
  -- entity events
  , spawn
  , spawnThen
  , destroy
  -- variadics support
  , mapStore
  , foldStore
  -- definition of world type
  , World
  , newWorld
  , worldRedux
) where

-- for exporting
import ReduxGame.Entities.Entity
import ReduxGame.Entities.Entities
import ReduxGame.Entities.EntityRedux
import ReduxGame.Entities.Store.Interactions
import ReduxGame.Entities.Store.Variadics

-- to support new definitions, linking all these bits together
import ReduxGame.Entities.Store.ComponentStore
import ReduxGame.Entities.Store.MapStore
import ReduxGame.Redux

type World = ComponentStore MapStore
newWorld = emptyComponents

worldRedux :: Redux World
worldRedux = entityRedux
