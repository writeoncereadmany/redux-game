module ReduxGame.Entities.Entity where

import Data.Typeable

class Typeable a => Component a

type EntityId = Integer

data Tagged a = Tagged EntityId a

data Property = forall c . Component c => Property c

data Entity = Entity [ Property ]

entity :: Entity
entity = Entity []

infixl 1 <-+

(<-+) :: Component c => Entity -> c -> Entity
(<-+) (Entity props) comp = Entity $ Property comp : props
