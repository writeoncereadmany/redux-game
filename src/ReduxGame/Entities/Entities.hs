module ReduxGame.Entities.Entities
  ( Entities
  , runEntities
  , getComponent
  , setComponent
  , evaluate
  , updateState
  , doApply2
  , create
  , listStore
  , ReduxGame.Entities.ListStore.EntityId
  , ReduxGame.Entities.Component.Component
  , ReduxGame.Entities.ComponentStore.emptyComponents
  ) where

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

updateState :: Store s => Entities a -> ComponentStore s -> ComponentStore s
updateState e c = snd $ runEntities e c

getComponent :: Component a => EntityId -> Entities (Maybe a)
getComponent entId = Entities $ \components -> (getComponent' entId components, components)

setComponent :: Component a => a -> EntityId -> Entities ()
setComponent a entId = Entities $ \components -> ((), setComponent' a entId components)

create :: Entity -> Entities EntityId
create entity = Entities $ \components -> createEntity' entity components

doApply2 :: (Component a, Component b) => ((a, b) -> (a, b)) -> Entities ()
doApply2 f = Entities $ \components -> ((), doApply2' f components)

listStore :: ComponentStore ListStore
listStore = emptyComponents
