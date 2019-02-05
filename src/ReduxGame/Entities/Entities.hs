module ReduxGame.Entities.Entities
  ( Entities
  , runEntities
  , evaluate
  , updateState
  , createEntity
  , destroyEntity
  , listStore
  , foldStore
  , Only (Only)
  , apply
  , applyM
  , Extractable
  , Persistable
  , EntityId
  , Component
  , ComponentStore
  , ListStore
  , MapStore
  , Tagged (Tagged)
  , emptyComponents
  , content
  ) where

import Data.Typeable

import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Store.ListStore
import ReduxGame.Entities.Store.MapStore
import ReduxGame.Entities.Entity
import ReduxGame.Entities.Store.ComponentStore
import ReduxGame.Entities.Store.Variadics

data Entities a = Entities { runEntities :: forall s . Store s => ComponentStore s -> (a, ComponentStore s)}

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

evaluate :: Store s => Entities a -> ComponentStore s -> a
evaluate e c = fst $ runEntities e c

updateState :: Store s => Entities () -> ComponentStore s -> ComponentStore s
updateState e c = snd $ runEntities e c

createEntity :: Entity -> Entities EntityId
createEntity entity = Entities $ \components -> createAll entity components

destroyEntity :: EntityId -> Entities ()
destroyEntity entity = Entities $ \components -> ((), destroyAll entity components)

updateEntity :: (Extractable a, Persistable b) => EntityId -> (a -> b) -> Entities ()
updateEntity entId f = Entities $ \components ->
  ((), maybe components (flip persist components . (: []) . Tagged entId . f) (extractWithId entId components))

listStore :: ComponentStore ListStore
listStore = emptyComponents
