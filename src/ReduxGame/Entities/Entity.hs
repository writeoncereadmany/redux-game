module ReduxGame.Entities.Entity where

import Data.Typeable
import Data.Maybe

class Typeable a => Component a

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
