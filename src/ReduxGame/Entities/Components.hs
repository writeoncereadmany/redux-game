module ReduxGame.Entities.Components where

import Data.Typeable
import Data.ConstrainedDynamic

type EntityId = Integer

class Typeable a => Component a

data Tagged a = Tagged EntityId a

type Store a = [ Tagged a ]

data Components where
  Components :: forall a . Component a => Store a -> Components

fromStore :: Component a => Components -> Maybe (Store a)
fromStore (Components b) = cast b

withId :: EntityId -> Store a -> Maybe a
withId entId [] = Nothing
withId entId ((Tagged entId' a) : as) =
  if entId == entId'
    then Just a
    else withId entId as

replaceComponent :: a -> EntityId -> Store a -> Store a
replaceComponent a entId [] = [ Tagged entId a ]
replaceComponent a entId (c@(Tagged entId' _) : cs) =
  if entId == entId'
    then (Tagged entId a) : cs
    else c : replaceComponent a entId cs
