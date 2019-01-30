module ReduxGame.Entities.Entities
  ( Entities
  , runEntities
  , evaluate
  , updateState
  , create
  , destroy
  , listStore
  , foldStore
  , Only (Only)
  , apply
  , Extractable
  , Updatable
  , ReduxGame.Entities.Entity.EntityId
  , ReduxGame.Entities.Entity.Component
  , ReduxGame.Entities.Store.ComponentStore.ComponentStore
  , ReduxGame.Entities.Store.ListStore.ListStore
  , ReduxGame.Entities.Store.ComponentStore.emptyComponents
  , ReduxGame.Entities.Store.Store.content
  ) where

import Data.Typeable

import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Store.ListStore
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

create :: Entity -> Entities EntityId
create entity = Entities $ \components -> createAll entity components

destroy :: EntityId -> Entities ()
destroy entity = Entities $ \components -> ((), destroyAll entity components)

sapply :: (Extractable a, Updatable b)
       => (a -> b)
       -> Entities ()
sapply f = Entities $ \cs -> ((), apply f cs)

listStore :: ComponentStore ListStore
listStore = emptyComponents
