module ReduxGame.Entities.Entities
  ( Entities
  , runEntities
  , getComponent
  , setComponent
  , evaluate
  , updateState
  , create
  , destroy
  , listStore
  , smap
  , Only (Only)
  , sapply
  , Extractable
  , Updatable
  , ReduxGame.Entities.Store.EntityId
  , ReduxGame.Entities.Component.Component
  , ReduxGame.Entities.ComponentStore.ComponentStore
  , ReduxGame.Entities.ListStore.ListStore
  , ReduxGame.Entities.ComponentStore.emptyComponents
  , ReduxGame.Entities.Store.content
  ) where

import Data.Typeable

import ReduxGame.Entities.Store
import ReduxGame.Entities.ListStore
import ReduxGame.Entities.Component
import ReduxGame.Entities.Entity
import ReduxGame.Entities.ComponentStore

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

getComponent :: Component a => EntityId -> Entities (Maybe a)
getComponent entId = Entities $ \components -> (getComponent' entId components, components)

setComponent :: Component a => a -> EntityId -> Entities ()
setComponent a entId = Entities $ \components -> ((), setComponent' a entId components)

create :: Entity -> Entities EntityId
create entity = Entities $ \components -> createEntity' entity components

destroy :: EntityId -> Entities ()
destroy entity = Entities $ \components -> ((), destroyEntity' entity components)

class Extractable a where
  extract :: forall s . Store s => ComponentStore s -> [ Tagged a ]

data Only a = Only a

unOnly :: Only a -> a
unOnly (Only a) = a

instance Component a => Extractable (Only a) where
  extract store = fmap Only <$> storeOf' store

instance (Component a, Component b) => Extractable (a, b) where
  extract store = combine2 (storeOf' store) (storeOf' store)

instance (Component a, Component b, Component c) => Extractable (a, b, c) where
  extract store = combine3 (storeOf' store) (storeOf' store) (storeOf' store)

class Updatable a where
  update :: forall s . Store s => [ Tagged a ] -> ComponentStore s -> ComponentStore s

instance Component a => Updatable (Only a) where
  update xs = bulkUpdate (fmap unOnly <$> xs)

instance (Component a, Component b) => Updatable (a, b) where
  update xs = bulkUpdate (fmap fst <$> xs)
            . bulkUpdate (fmap snd <$> xs)

smap :: Extractable a
     => (a -> b)
     -> Entities [ b ]
smap f = Entities $ \cs -> (f <$> content <$> extract cs, cs)

sapply :: (Extractable a, Updatable b)
       => (a -> b)
       -> Entities ()
sapply f = Entities $ \cs -> ((), update (fmap f <$> (extract cs)) cs)

listStore :: ComponentStore ListStore
listStore = emptyComponents
