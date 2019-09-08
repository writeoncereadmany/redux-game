{-# OPTIONS_GHC -F -pgmF htfpp #-}

module EntitiesTest (htf_thisModulesTests) where

import ReduxGame.Entities
import ReduxGame.Entities.Entities
import ReduxGame.Entities.Store.ComponentStore
import Test.Framework

instance Component String
instance Component Bool

initialStore :: World
initialStore = emptyComponents

data X = X Int deriving (Eq, Show, Component)
data Y = Y Int deriving (Eq, Show, Component)

swap :: (X, Y) -> (X, Y)
swap (X x, Y y) = (X y, Y x)

setupData :: Entities (EntityId, EntityId, EntityId, EntityId)
setupData = do
  a <- doCreateEntity (entity <-+ X 3 <-+ Y 5)
  b <- doCreateEntity (entity <-+ X 2)
  c <- doCreateEntity (entity <-+ Y 4)
  d <- doCreateEntity (entity <-+ X 6 <-+ Y 13)
  return (a, b, c, d)

test_can_parallel_apply = do
  let ((a,b,c,d), newState) = runEntities setupData initialStore
  let newState' = apply swap newState
  assertEqual [Tagged a (X 5), Tagged b (X 2), Tagged d (X 13)] (storeOf newState')
  assertEqual [Tagged a (Y 3), Tagged c (Y 4), Tagged d (Y 6)] (storeOf newState')

createAndDestroy = do
  a <- doCreateEntity (entity <-+ X 5)
  b <- doCreateEntity (entity <-+ Y 3)
  doDestroyEntity a
  return (a, b)

test_can_delete_entities = do
  let ((a, b), newState) = runEntities createAndDestroy initialStore
  assertEqual [Tagged b (Y 3)] (storeOf newState)
