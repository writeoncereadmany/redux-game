module ReduxGame.Entities.Entities
  ( Entities
  , runEntities
  , evaluate
  , updateState
  , doCreateEntity
  , doDestroyEntity
  , doUpdateEntity
  , mapStore
  , apply
  , applyM
  , EntityId
  , Component
  , ComponentStore
  , MapStore
  , Tagged (Tagged)
  , emptyComponents
  , content
  ) where

import Data.Typeable

import ReduxGame.Entities.Component
import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Store.MapStore
import ReduxGame.Entities.Entity
import ReduxGame.Entities.Store.ComponentStore

data Entities a = Entities { runEntities :: forall c . Components c => c -> (a, c)}

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

evaluate :: Components c => Entities a -> c -> a
evaluate e c = fst $ runEntities e c

updateState :: Components c => Entities () -> c -> c
updateState e c = snd $ runEntities e c

doCreateEntity :: Entity -> Entities EntityId
doCreateEntity entity = Entities $ \components -> createEntity entity components

doDestroyEntity :: EntityId -> Entities ()
doDestroyEntity entity = Entities $ \components -> ((), destroyEntity entity components)

doUpdateEntity :: (Component a, Component b) => EntityId -> (a -> b) -> Entities ()
doUpdateEntity entId f = Entities $ \components ->
  ((), maybe components (flip setAll components . (: []) . Tagged entId . f) (getById entId components))
