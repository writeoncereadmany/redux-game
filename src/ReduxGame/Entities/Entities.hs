module ReduxGame.Entities.Entities
  ( Entities
  , runEntities
  , getComponent
  , setComponent
  , ReduxGame.Entities.Store.EntityId
  , ReduxGame.Entities.ComponentStore.Component
  , ReduxGame.Entities.ComponentStore.emptyStore
  ) where

import ReduxGame.Entities.Store
import ReduxGame.Entities.ComponentStore

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

getComponent :: Component a => EntityId -> Entities (Maybe a)
getComponent entityId = Entities $ \components ->
  let store = storeOf components
      component = withId entityId store
   in (component, components)

setComponent :: Component a => a -> EntityId -> Entities ()
setComponent component entityId = Entities $ \components ->
  let store = storeOf components
      store' = replaceComponent entityId component store
      components' = replaceStore store' components
   in ((), components')
