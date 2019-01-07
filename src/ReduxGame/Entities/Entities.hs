module ReduxGame.Entities.Entities
  ( Entities
  , runEntities
  , getComponent
  , setComponent
  , evaluate
  , updateState
  , doApply2
  , ReduxGame.Entities.ListStore.EntityId
  , ReduxGame.Entities.ComponentStore.Component
  , ReduxGame.Entities.ComponentStore.emptyComponents
  ) where

import ReduxGame.Entities.ListStore
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

evaluate :: Entities a -> ComponentStore -> a
evaluate e c = fst $ runEntities e c

updateState :: Entities a -> ComponentStore -> ComponentStore
updateState e c = snd $ runEntities e c

getComponent :: Component a => EntityId -> Entities (Maybe a)
getComponent entId = Entities $ \components -> getComponent' entId components

setComponent :: Component a => a -> EntityId -> Entities ()
setComponent a entId = Entities $ \components -> ((), setComponent' a entId components)

doApply2 :: (Component a, Component b) => ((a, b) -> (a, b)) -> Entities ()
doApply2 f = Entities $ \components -> ((), doApply2' f components)
