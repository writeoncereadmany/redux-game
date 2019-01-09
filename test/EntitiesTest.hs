{-# OPTIONS_GHC -F -pgmF htfpp #-}

module EntitiesTest (htf_thisModulesTests) where

import ReduxGame.Entities.Entity
import ReduxGame.Entities.Entities
import Test.Framework

instance Component String
instance Component Bool

initialStore = listStore

test_can_set_and_retrieve_for_matching_ids = do
  let (value, _) = runEntities (do { setComponent "Hello!" 12; getComponent 12}) initialStore
  assertEqual (Just "Hello!") value

test_retrieve_fails_when_types_dont_match = do
  let (value, _) = runEntities (do { setComponent False 12; getComponent 12}) initialStore
  assertEqual (Nothing :: Maybe String) value

test_retrieve_fails_when_entity_ids_dont_match = do
  let (value, _) = runEntities (do { setComponent "Hello!" 15; getComponent 12}) initialStore
  assertEqual (Nothing :: Maybe String) value

test_can_store_and_retrieve_multiple_entity_ids = do
  let (value, _) = runEntities setAndGetMultipleComponents initialStore
  assertEqual (Just ("Hello!", True)) value where
    setAndGetMultipleComponents = do
      setComponent "Hello!" 12
      setComponent True 12
      first <- getComponent 12
      second <- getComponent 12
      return $ case (first, second) of
        (Just a, Just b) -> Just (a, b)
        _ -> Nothing

data X = X Int deriving (Eq, Show, Component)
data Y = Y Int deriving (Eq, Show, Component)

swap :: (X, Y) -> (X, Y)
swap (X x, Y y) = (X y, Y x)

setupData :: Entities (EntityId, EntityId, EntityId, EntityId)
setupData = do
  a <- create (entity <-+ X 3 <-+ Y 5)
  b <- create (entity <-+ X 2)
  c <- create (entity <-+ Y 4)
  d <- create (entity <-+ X 6 <-+ Y 13)
  return (a, b, c, d)

test_can_parallel_apply' = do
  let ((a, b, c, d), newState) = runEntities (do setupData) initialStore
  assertEqual (Just (X 3)) (evaluate (getComponent a) newState)
  assertEqual (Just (Y 5)) (evaluate (getComponent a) newState)
  assertEqual (Just (X 2)) (evaluate (getComponent b) newState)
  assertEqual (Nothing :: Maybe Y) (evaluate (getComponent b) newState)
  assertEqual (Nothing :: Maybe X) (evaluate (getComponent c) newState)
  assertEqual (Just (Y 4)) (evaluate (getComponent c) newState)
  assertEqual (Just (X 6)) (evaluate (getComponent d) newState)
  assertEqual (Just (Y 13)) (evaluate (getComponent d) newState)

test_can_parallel_apply = do
  let ((a,b,c,d), newState) = runEntities (do { ids <- setupData; doApply2 swap; return ids }) initialStore
  assertEqual (Just (X 5)) (evaluate (getComponent a) newState)
  assertEqual (Just (Y 3)) (evaluate (getComponent a) newState)
  assertEqual (Just (X 2)) (evaluate (getComponent b) newState)
  assertEqual (Nothing :: Maybe Y) (evaluate (getComponent b) newState)
  assertEqual (Nothing :: Maybe X) (evaluate (getComponent c) newState)
  assertEqual (Just (Y 4)) (evaluate (getComponent c) newState)
  assertEqual (Just (X 13)) (evaluate (getComponent d) newState)
  assertEqual (Just (Y 6)) (evaluate (getComponent d) newState)

createAndDestroy = do
  a <- create (entity <-+ X 5)
  b <- create (entity <-+ Y 3)
  destroy a
  return (a, b)

test_can_delete_entities = do
  let ((a, b), newState) = runEntities createAndDestroy initialStore
  assertEqual (Just (Y 3)) (evaluate (getComponent b) newState)
  assertEqual (Nothing :: Maybe X) (evaluate (getComponent a) newState)
