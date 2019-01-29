module ReduxGame.Entities.Entity where

import Data.Typeable

class Typeable a => Component a

type EntityId = Integer

data Tagged a = Tagged EntityId a

instance Eq a => Eq (Tagged a) where
  (Tagged id_a a) == (Tagged id_b b) = id_a == id_b && a == b

instance Show a => Show (Tagged a) where
  show (Tagged id_a a) = show (id_a, a)

data Property = forall c . Component c => Property c

data Entity = Entity [ Property ]

entity :: Entity
entity = Entity []

infixl 1 <-+

(<-+) :: Component c => Entity -> c -> Entity
(<-+) (Entity props) comp = Entity $ Property comp : props
