module ReduxGame.Entities.Store.ComponentStore
 ( ComponentStore
 , emptyComponents
 , storeOf
 , replaceStore
 , merge
 , createAll
 , destroyAll
 ) where

import Data.Maybe
import Data.Typeable
import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Entity

data DynStore s where
  DynStore :: forall s a . (Store s, Component a) => s a -> DynStore s

data ComponentStore s = ComponentStore EntityId [ DynStore s ]

emptyComponents :: ComponentStore s
emptyComponents = ComponentStore 0 []

storeOf :: (Store s, Component a) => ComponentStore s -> [ Tagged a ]
storeOf = components . storeOf' where

replaceStore :: (Store s, Component a) => s a -> ComponentStore s -> ComponentStore s
replaceStore newStore (ComponentStore nextId oldStores) =
  ComponentStore nextId $ replaceStore' newStore oldStores where
    replaceStore' newStore [] = [ DynStore newStore ]
    replaceStore' newStore (oldStore : oldStores) =
      if (typesMatch newStore (fromStore oldStore))
        then (DynStore newStore) : oldStores
        else oldStore : replaceStore' newStore oldStores

merge :: (Store s, Component a) => [ Tagged a ] -> ComponentStore s -> ComponentStore s
merge xs cs = replaceStore (mergeComponents xs $ storeOf' cs) cs

createAll :: (Store s) => Entity -> ComponentStore s -> (EntityId, ComponentStore s)
createAll (Entity properties) store@(ComponentStore nextId _) =
  let newStore = (foldr (\(Property p) s -> merge [ Tagged nextId p] s) store properties)
   in incrementId newStore where
     incrementId (ComponentStore nextId components) = (nextId, ComponentStore (succ nextId) components)

destroyAll :: (Store s) => EntityId -> ComponentStore s -> ComponentStore s
destroyAll entId (ComponentStore nextId stores) = ComponentStore nextId (deleteFrom <$> stores) where
  deleteFrom (DynStore as) = DynStore $ delete entId as

typesMatch :: a -> Maybe a -> Bool
typesMatch _ x = isJust x

fromStore :: (Store s, Component a) => DynStore s -> Maybe (s a)
fromStore (DynStore b) = cast b

storeOf' :: (Store s, Component a) => ComponentStore s -> s a
storeOf' (ComponentStore _ stores) = storeOf'' stores where
  storeOf'' [] = emptyStore
  storeOf'' (x:xs) = case fromStore x of
    Just a  -> a
    Nothing -> storeOf'' xs
