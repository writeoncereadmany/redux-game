module ReduxGame.Entities.Store.ComponentStore
 ( ComponentStore
 , emptyComponents
 , storeOf
 , storeOf'
 , replaceStore
 , merge
 , createAll
 , destroyAll
 , getComponent
 ) where

import Data.Maybe
import Data.Typeable
import qualified Data.Map as M
import ReduxGame.Entities.Store.Store as S
import ReduxGame.Entities.Component
import ReduxGame.Entities.Entity

data DynStore s where
  DynStore :: forall s a . (Store s, Component a) => s a -> DynStore s

data ComponentStore s = ComponentStore EntityId (M.Map TypeRep (DynStore s))

emptyComponents :: ComponentStore s
emptyComponents = ComponentStore 0 M.empty

storeOf :: (Store s, Component a) => ComponentStore s -> [ Tagged a ]
storeOf = components . storeOf'

replaceStore :: forall a s . (Store s, Component a) => s a -> ComponentStore s -> ComponentStore s
replaceStore newStore (ComponentStore nextId oldStores) =
  let typeKey = typeRep (Proxy :: Proxy a)
   in ComponentStore nextId (M.insert typeKey (DynStore newStore) oldStores)

merge :: (Store s, Component a) => [ Tagged a ] -> ComponentStore s -> ComponentStore s
merge xs cs = replaceStore (mergeComponents xs $ storeOf' cs) cs

update :: (Store s, Component a) => [ Tagged (Maybe a)] -> ComponentStore s -> ComponentStore s
update maybes cs = replaceStore (S.updateComponents maybes $ storeOf' cs) cs

createAll :: (Store s) => Entity -> ComponentStore s -> (EntityId, ComponentStore s)
createAll (Entity properties) store@(ComponentStore nextId _) =
  let newStore = (foldr (\(Property p) s -> merge [ Tagged nextId p] s) store properties)
   in incrementId newStore where
     incrementId (ComponentStore nextId components) = (nextId, ComponentStore (succ nextId) components)

destroyAll :: (Store s) => EntityId -> ComponentStore s -> ComponentStore s
destroyAll entId (ComponentStore nextId stores) = ComponentStore nextId (deleteFrom <$> stores) where
  deleteFrom (DynStore as) = DynStore $ delete entId as

getComponent :: (Component a, Store s) => EntityId -> ComponentStore s -> Maybe a
getComponent entId cs = withId entId $ storeOf' cs

fromStore :: (Store s, Component a) => DynStore s -> Maybe (s a)
fromStore (DynStore b) = cast b

storeOf' :: forall a s . (Store s, Component a) => ComponentStore s -> s a
storeOf' (ComponentStore _ stores) = let typeKey = typeRep (Proxy :: Proxy a)
  in case M.lookup typeKey stores >>= fromStore of
    (Just store) -> store
    (Nothing) -> emptyStore

instance Store s => Components (ComponentStore s) where
  allComponents = storeOf
  componentById = getComponent
  updateComponents = update
  createEntity = createAll
  destroyEntity = destroyAll
