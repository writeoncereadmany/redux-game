module ReduxGame.Entities.Store where

import Data.Typeable

type EntityId = Integer

data Tagged a = Tagged EntityId a

class Typeable t => Store t where
  withId :: EntityId -> t a -> Maybe a
  replaceComponent :: EntityId -> a -> t a -> t a
  apply2 :: ((a, b) -> (a, b)) -> t a -> t b -> (t a, t b)
  emptyStore :: t a
  delete :: EntityId -> t a -> t a
