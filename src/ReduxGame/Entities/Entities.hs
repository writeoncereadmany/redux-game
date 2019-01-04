module ReduxGame.Entities.Entities
  ( Entities
  , runEntities
  , getComponent
  , setComponent
  , evaluate
  , updateState
  , doApply2
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

evaluate :: Entities a -> ComponentStore -> a
evaluate e c = fst $ runEntities e c

updateState :: Entities a -> ComponentStore -> ComponentStore
updateState e c = snd $ runEntities e c

doApply2 :: (Component a, Component b) => ((a, b) -> (a, b)) -> Entities ()
doApply2 f = Entities $ \components -> ((), apply2 f components)

apply2 :: forall a b . (Component a, Component b)
       => ((a, b) -> (a, b))
       -> ComponentStore
       -> ComponentStore
apply2 f components = let
     as = storeOf components
     bs = storeOf components
     (as', bs') = apply2'' as bs
  in replaceStore as' $ replaceStore bs' $ components where
    apply2'' :: Store a -> Store b -> (Store a, Store b)
    apply2'' (Store as) (Store bs) = let (as', bs') = apply2' as bs in (Store as', Store bs')
    apply2' :: [ Tagged a ] -> [ Tagged b] -> ([ Tagged a ], [ Tagged b ])
    apply2' [] bs = ([], bs)
    apply2' as [] = (as, [])
    apply2' as@(firsta@(Tagged id_a a):resta) bs@(firstb@(Tagged id_b b):restb) = case compare id_a id_b of
      LT -> let (as', bs') = apply2' resta bs
             in (firsta:as', bs')
      GT -> let (as', bs') = apply2' as restb
             in (as', firstb:bs')
      EQ -> let (a', b') = f (a, b)
                (as', bs') = apply2' resta restb
             in ((Tagged id_a a'):as', (Tagged id_b b'):bs')
