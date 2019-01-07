module ReduxGame.Entities.ComponentStore where

import Data.Maybe
import Data.Typeable
import ReduxGame.Entities.ListStore

class Typeable a => Component a

data DynStore where
  DynStore :: forall a . Component a => ListStore a -> DynStore

fromStore :: Component a => DynStore -> Maybe (ListStore a)
fromStore (DynStore b) = cast b

data ComponentStore = ComponentStore [ DynStore ]

emptyComponents :: ComponentStore
emptyComponents = ComponentStore []

storeOf :: Component a => ComponentStore -> ListStore a
storeOf (ComponentStore stores) = storeOf' stores where
  storeOf' [] = emptyStore
  storeOf' (x:xs) = case fromStore x of
    Just a  -> a
    Nothing -> storeOf' xs

typesMatch :: a -> Maybe a -> Bool
typesMatch _ x = isJust x

replaceStore :: Component a => ListStore a -> ComponentStore -> ComponentStore
replaceStore newStore (ComponentStore oldStores) =
  ComponentStore $ replaceStore' newStore oldStores where
    replaceStore' :: Component a => ListStore a -> [ DynStore ] -> [ DynStore ]
    replaceStore' newStore [] = [ DynStore newStore ]
    replaceStore' newStore (oldStore : oldStores) =
      if (typesMatch newStore (fromStore oldStore))
        then (DynStore newStore) : oldStores
        else oldStore : replaceStore' newStore oldStores

getComponent' :: Component a => EntityId -> ComponentStore -> (Maybe a, ComponentStore)
getComponent' entityId components =
  let store = storeOf components
      component = withId entityId store
   in (component, components)

setComponent' :: Component a => a -> EntityId -> ComponentStore -> ComponentStore
setComponent' component entityId components =
  let store = storeOf components
      store' = replaceComponent entityId component store
   in replaceStore store' components

doApply2' :: (Component a, Component b) => ((a, b) -> (a, b)) -> ComponentStore -> ComponentStore
doApply2' f components = let (as', bs') = apply2 f (storeOf components) (storeOf components)
                         in replaceStore as' $ replaceStore bs' components
