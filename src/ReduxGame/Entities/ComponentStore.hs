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

data Entities a = Entities { runEntities :: ComponentStore -> (a, ComponentStore)}

instance Functor Entities where
  fmap f entities = Entities $ \components ->
    let (val, components') = runEntities entities components
     in (f val, components')

instance Applicative Entities where
  pure a = Entities $ \components -> (a, components)
  f <*> a = Entities $ \components ->
    let (f', components') = runEntities f components
        (a', components'') = runEntities a components
     in (f' a', components'')

instance Monad Entities where
  return = pure
  entities >>= f = Entities $ \components ->
    let (val, components') = runEntities entities components
     in runEntities (f val) components'

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

getComponent :: Component a => EntityId -> Entities (Maybe a)
getComponent entityId = Entities $ \components ->
  let store = storeOf components
      component = withId entityId store
   in (component, components)

setComponent :: Component a => a -> EntityId -> Entities ()
setComponent component entityId = Entities $ \components ->
  let store = storeOf components
      store' = replaceComponent component entityId store
      components' = replaceStore store' components
   in ((), components')
