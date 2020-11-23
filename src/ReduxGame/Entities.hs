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
  , firstComponent
  -- definition of world type
  , World
  , newWorld
  , worldRedux
) where

-- for exporting
import ReduxGame.Entities.Component
import ReduxGame.Entities.Components
import ReduxGame.Entities.Entities
import ReduxGame.Entities.EntityRedux

-- to support new definitions, linking all these bits together
import ReduxGame.Redux

type World = ComponentStore MapStore
newWorld = emptyComponents

worldRedux :: Redux World
worldRedux = entityRedux
