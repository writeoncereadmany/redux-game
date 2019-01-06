module ReduxGame.Entities.Store where

type EntityId = Integer

data Tagged a = Tagged EntityId a

class Store t a where
  withId :: EntityId -> t a -> Maybe a
  replaceComponent :: EntityId -> a -> t a -> t a
  apply2 :: ((a, b) -> (a, b)) -> t a -> t b -> (t a, t b)
  emptyStore :: t a
