{-# OPTIONS_GHC -F -pgmF htfpp #-}

module EntitiesTest (htf_thisModulesTests) where

import ReduxGame.Entities.ComponentStore
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
