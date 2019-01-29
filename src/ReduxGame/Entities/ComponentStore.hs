module ReduxGame.Entities.ComponentStore where

import Data.Maybe
import Data.Typeable
import ReduxGame.Entities.Store
import ReduxGame.Entities.Entity

data DynStore s where
  DynStore :: forall s a . (Store s, Component a) => s a -> DynStore s

fromStore :: (Store s, Component a) => DynStore s -> Maybe (s a)
fromStore (DynStore b) = cast b

data ComponentStore s = ComponentStore EntityId [ DynStore s ]

emptyComponents :: ComponentStore s
emptyComponents = ComponentStore 0 []

storeOf :: (Store s, Component a) => ComponentStore s -> s a
storeOf (ComponentStore _ stores) = storeOf' stores where
  storeOf' [] = emptyStore
  storeOf' (x:xs) = case fromStore x of
    Just a  -> a
    Nothing -> storeOf' xs

storeOf' :: (Store s, Component a) => ComponentStore s -> [ Tagged a ]
storeOf' = components . storeOf

typesMatch :: a -> Maybe a -> Bool
typesMatch _ x = isJust x

replaceStore :: (Store s, Component a) => s a -> ComponentStore s -> ComponentStore s
replaceStore newStore (ComponentStore nextId oldStores) =
  ComponentStore nextId $ replaceStore' newStore oldStores where
    replaceStore' newStore [] = [ DynStore newStore ]
    replaceStore' newStore (oldStore : oldStores) =
      if (typesMatch newStore (fromStore oldStore))
        then (DynStore newStore) : oldStores
        else oldStore : replaceStore' newStore oldStores

bulkUpdate :: (Store s, Component a) => [ Tagged a ] -> ComponentStore s -> ComponentStore s
bulkUpdate xs cs = replaceStore (mergeComponents xs $ storeOf cs) cs

getComponent' :: (Store s, Component a) => EntityId -> ComponentStore s -> Maybe a
getComponent' entityId components =
  let store = storeOf components
   in withId entityId store

setComponent' :: (Store s, Component a) => a -> EntityId -> ComponentStore s -> ComponentStore s
setComponent' component entityId components =
  let store = storeOf components
      store' = mergeComponents [ Tagged entityId component ] store
   in replaceStore store' components

createEntity' :: (Store s) => Entity -> ComponentStore s -> (EntityId, ComponentStore s)
createEntity' (Entity properties) store@(ComponentStore nextId _) =
  let newStore = (foldr (\(Property p) s -> setComponent' p nextId s) store properties)
   in incrementId newStore where
     incrementId (ComponentStore nextId components) = (nextId, ComponentStore (succ nextId) components)

destroyEntity' :: (Store s) => EntityId -> ComponentStore s -> ComponentStore s
destroyEntity' entId (ComponentStore nextId stores) = ComponentStore nextId (delete' <$> stores) where
  delete' (DynStore as) = DynStore $ delete entId as
