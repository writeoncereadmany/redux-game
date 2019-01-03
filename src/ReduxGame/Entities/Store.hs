module ReduxGame.Entities.Store where

type EntityId = Integer

data Tagged a = Tagged EntityId a

data Store a = Store [ Tagged a ]

withId :: EntityId -> Store a -> Maybe a
withId entId (Store xs) = withId' xs where
  withId' [] = Nothing
  withId' ((Tagged entId' a) : as) =
    if entId == entId'
      then Just a
      else withId' as

replaceComponent :: a -> EntityId -> Store a -> Store a
replaceComponent a entId (Store xs) = Store $ replaceComponent' xs where
  replaceComponent' [] = [ Tagged entId a ]
  replaceComponent' (c@(Tagged entId' _) : cs) =
    if entId == entId'
      then (Tagged entId a) : cs
      else c : replaceComponent' cs
