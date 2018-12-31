module ReduxGame.Entities.ComponentStore where

import Data.Maybe
import Data.Typeable
import Data.ConstrainedDynamic

type EntityId = Integer

class Typeable a => Component a

data Tagged a = Tagged EntityId a

data DynamicStore where
  DynamicStore :: forall a . Component a => [ Tagged a ] -> DynamicStore

toStore :: Component a => [ Tagged a ] -> DynamicStore
toStore = DynamicStore

fromStore :: Component a => DynamicStore -> Maybe [ Tagged a ]
fromStore (DynamicStore b) = cast b

data ComponentStore = ComponentStore [ DynamicStore ]

emptyStore :: ComponentStore
emptyStore = ComponentStore []

storeOf :: Component a => ComponentStore -> [ Tagged a ]
storeOf (ComponentStore stores) = storeOf' stores where
  storeOf' [] = []
  storeOf' (x:xs) = case fromStore x of
    Just a  -> a
    Nothing -> storeOf' xs

typesMatch :: a -> Maybe a -> Bool
typesMatch _ x = isJust x

replaceStore :: Component a => [ Tagged a] -> ComponentStore -> ComponentStore
replaceStore newStore (ComponentStore oldStores) =
  ComponentStore $ replaceStore' newStore oldStores where
    replaceStore' :: Component a => [Tagged a] -> [DynamicStore] -> [DynamicStore]
    replaceStore' newStore [] = [ toStore newStore ]
    replaceStore' newStore (oldStore : oldStores) =
      if (typesMatch newStore (fromStore oldStore))
        then (toStore newStore) : oldStores
        else oldStore : replaceStore' newStore oldStores

withId :: EntityId -> [ Tagged a ] -> Maybe a
withId entId [] = Nothing
withId entId ((Tagged entId' a) : as) =
  if entId == entId'
    then Just a
    else withId entId as

replaceComponent :: a -> EntityId -> [ Tagged a] -> [Tagged a]
replaceComponent a entId [] = [ Tagged entId a ]
replaceComponent a entId (c@(Tagged entId' _) : cs) =
  if entId == entId'
    then (Tagged entId a) : cs
    else c : replaceComponent a entId cs
