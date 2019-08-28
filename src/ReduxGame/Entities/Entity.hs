module ReduxGame.Entities.Entity where

import Data.Typeable
import Data.Maybe

data Tagged a = Tagged EntityId a

class Components c where
  allComponents :: Component a => c -> [ Tagged a ]
  componentById :: Component a => EntityId -> c -> Maybe a
  updateComponents :: Component a => [ Tagged a ] -> c -> c

class Typeable a => Component a where
  getAll :: Components c => c -> [ Tagged a ]
  getAll = allComponents
  getById :: Components c => EntityId -> c -> Maybe a
  getById = componentById
  setAll :: Components c => [ Tagged a ] -> c -> c
  setAll = updateComponents
  setById :: Components c => EntityId -> a -> c -> c
  setById e a = updateComponents [ Tagged e a ]

class Default a where
  defaultValue :: a
  orDefault :: Maybe a -> a
  orDefault = fromMaybe defaultValue

type EntityId = Integer

data Property = forall c . Component c => Property c

data Entity = Entity [ Property ]

entity :: Entity
entity = Entity []

infixl 1 <-+

(<-+) :: Component c => Entity -> c -> Entity
(<-+) (Entity props) comp = Entity $ Property comp : props

infixl 1 <++

(<++) :: Entity -> Entity -> Entity
(<++) (Entity props1) (Entity props2) = Entity $ props1 ++ props2
