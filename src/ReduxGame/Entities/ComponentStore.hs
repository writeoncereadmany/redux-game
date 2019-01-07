module ReduxGame.Entities.ComponentStore where

import Data.Maybe
import Data.Typeable
import ReduxGame.Entities.Store
import ReduxGame.Entities.ListStore

class Typeable a => Component a

data DynStore s where
  DynStore :: forall s a . (Store s, Component a) => s a -> DynStore s

fromStore :: (Store s, Component a) => DynStore s -> Maybe (s a)
fromStore (DynStore b) = cast b

data ComponentStore s = ComponentStore [ DynStore s ]

emptyComponents :: ComponentStore s
emptyComponents = ComponentStore []

storeOf :: (Store s, Component a) => ComponentStore s -> s a
storeOf (ComponentStore stores) = storeOf' stores where
  storeOf' [] = emptyStore
  storeOf' (x:xs) = case fromStore x of
    Just a  -> a
    Nothing -> storeOf' xs

typesMatch :: a -> Maybe a -> Bool
typesMatch _ x = isJust x

replaceStore :: (Store s, Component a) => s a -> ComponentStore s -> ComponentStore s
replaceStore newStore (ComponentStore oldStores) =
  ComponentStore $ replaceStore' newStore oldStores where
    replaceStore' newStore [] = [ DynStore newStore ]
    replaceStore' newStore (oldStore : oldStores) =
      if (typesMatch newStore (fromStore oldStore))
        then (DynStore newStore) : oldStores
        else oldStore : replaceStore' newStore oldStores

getComponent' :: (Store s, Component a) => EntityId -> ComponentStore s -> Maybe a
getComponent' entityId components =
  let store = storeOf components
   in withId entityId store
   
setComponent' :: (Store s, Component a) => a -> EntityId -> ComponentStore s -> ComponentStore s
setComponent' component entityId components =
  let store = storeOf components
      store' = replaceComponent entityId component store
   in replaceStore store' components

doApply2' :: (Store s, Component a, Component b) => ((a, b) -> (a, b)) -> ComponentStore s -> ComponentStore s
doApply2' f components = let (as', bs') = apply2 f (storeOf components) (storeOf components)
                         in replaceStore as' $ replaceStore bs' components
