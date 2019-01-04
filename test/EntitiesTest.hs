{-# OPTIONS_GHC -F -pgmF htfpp #-}

module EntitiesTest (htf_thisModulesTests) where

import ReduxGame.Entities.Entities
import Test.Framework

instance Component String
instance Component Bool

test_can_set_and_retrieve_for_matching_ids = do
  let (value, _) = runEntities (do { setComponent "Hello!" 12; getComponent 12}) emptyStore
  assertEqual (Just "Hello!") value

test_retrieve_fails_when_types_dont_match = do
  let (value, _) = runEntities (do { setComponent False 12; getComponent 12}) emptyStore
  assertEqual (Nothing :: Maybe String) value

test_retrieve_fails_when_entity_ids_dont_match = do
  let (value, _) = runEntities (do { setComponent "Hello!" 15; getComponent 12}) emptyStore
  assertEqual (Nothing :: Maybe String) value

test_can_store_and_retrieve_multiple_entity_ids = do
  let (value, _) = runEntities setAndGetMultipleComponents emptyStore
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

setupData :: Entities ()
setupData = do
  setComponent (X 3) 1
  setComponent (Y 5) 1
  setComponent (X 2) 2
  setComponent (Y 4) 3
  setComponent (X 6) 4
  setComponent (Y 13) 4

test_can_parallel_apply' = do
  let newState = updateState (do setupData) emptyStore
  assertEqual (Just (X 3)) (evaluate (getComponent 1) newState)
  assertEqual (Just (Y 5)) (evaluate (getComponent 1) newState)
  assertEqual (Just (X 2)) (evaluate (getComponent 2) newState)
  assertEqual (Nothing :: Maybe Y) (evaluate (getComponent 2) newState)
  assertEqual (Nothing :: Maybe X) (evaluate (getComponent 3) newState)
  assertEqual (Just (Y 4)) (evaluate (getComponent 3) newState)
  assertEqual (Just (X 6)) (evaluate (getComponent 4) newState)
  assertEqual (Just (Y 13)) (evaluate (getComponent 4) newState)


test_can_parallel_apply = do
  let newState = updateState (do { setupData; doApply2 swap; }) emptyStore
  assertEqual (Just (X 5)) (evaluate (getComponent 1) newState)
  assertEqual (Just (Y 3)) (evaluate (getComponent 1) newState)
  assertEqual (Just (X 2)) (evaluate (getComponent 2) newState)
  assertEqual (Nothing :: Maybe Y) (evaluate (getComponent 2) newState)
  assertEqual (Nothing :: Maybe X) (evaluate (getComponent 3) newState)
  assertEqual (Just (Y 4)) (evaluate (getComponent 3) newState)
  assertEqual (Just (X 13)) (evaluate (getComponent 4) newState)
  assertEqual (Just (Y 6)) (evaluate (getComponent 4) newState)
