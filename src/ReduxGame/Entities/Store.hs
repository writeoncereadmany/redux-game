module ReduxGame.Entities.Store where

type EntityId = Integer

data Tagged a = Tagged EntityId a

data Store a = Store [ Tagged a ]

withId :: EntityId -> Store a -> Maybe a
withId entId (Store xs) = withId' xs where
  withId' [] = Nothing
  withId' ((Tagged entId' a) : as) =
    case compare entId entId' of
      LT -> withId' as
      EQ -> Just a
      GT -> Nothing

replaceComponent :: EntityId -> a -> Store a -> Store a
replaceComponent entId a (Store xs) = Store $ replaceComponent' xs where
  replaceComponent' [] = [ Tagged entId a ]
  replaceComponent' cs@(c@(Tagged entId' _) : restc) =
    case compare entId entId' of
      LT -> (Tagged entId a) : cs
      EQ -> (Tagged entId a) : restc
      GT -> c : replaceComponent' restc
