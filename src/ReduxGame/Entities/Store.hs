module ReduxGame.Entities.Store where

import Data.Typeable
import ReduxGame.Entities.Entity

instance Functor Tagged where
  fmap f (Tagged entId a) = Tagged entId (f a)

content :: Tagged a -> a
content (Tagged _ a) = a

class Typeable t => Store t where
  withId :: EntityId -> t a -> Maybe a
  mergeComponents :: [ Tagged a ] -> t a -> t a
  components :: t a -> [ Tagged a ]
  emptyStore :: t a
  delete :: EntityId -> t a -> t a

combine2 :: [ Tagged a ] -> [ Tagged b ] -> [ Tagged (a, b) ]
combine2 _ [] = []
combine2 [] _ = []
combine2 all_as@(Tagged a_id a : rest_as) all_bs@(Tagged b_id b : rest_bs)
  | a_id == b_id = Tagged a_id (a, b) : combine2 rest_as rest_bs
  | a_id > b_id = combine2 all_as rest_bs
  | a_id < b_id = combine2 rest_as all_bs

combine3 :: [ Tagged a ] -> [ Tagged b ] -> [Tagged c ] -> [ Tagged (a, b, c)]
combine3 as bs cs = (flatten <$>) <$> (combine2 as (combine2 bs cs)) where
  flatten (a, (b, c)) = (a, b, c)

combine4 :: [ Tagged a ] -> [ Tagged b ] -> [Tagged c ] -> [ Tagged d ] -> [ Tagged (a, b, c, d)]
combine4 as bs cs ds = (flatten <$>) <$> (combine2 (combine2 as bs) (combine2 cs ds)) where
  flatten ((a, b), (c, d)) = (a, b, c, d)
