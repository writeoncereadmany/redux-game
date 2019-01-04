module ReduxGame.Entities.Store where

type EntityId = Integer

data Tagged a = Tagged EntityId a

class Store t a where
  swithId :: EntityId -> t a -> Maybe a
  sreplaceComponent :: EntityId -> a -> t a -> t a
  sapply2 :: ((a, b) -> (a, b)) -> t a -> t b -> (t a, t b)
  semptyStore :: t a
