module ReduxGame.Entities.Store.Store where

import Data.Typeable
import ReduxGame.Entities.Entity

instance Eq a => Eq (Tagged a) where
  (Tagged id_a a) == (Tagged id_b b) = id_a == id_b && a == b

instance Show a => Show (Tagged a) where
  show (Tagged id_a a) = show (id_a, a)

instance Foldable Tagged where
  foldMap f (Tagged entId a) = f a

instance Traversable Tagged where
  traverse f (Tagged entId a) = pure (Tagged entId) <*> f a

content :: Tagged a -> a
content (Tagged _ a) = a

idOf :: Tagged a -> EntityId
idOf (Tagged entId _) = entId

toPair :: Tagged a -> (EntityId, a)
toPair (Tagged i a) = (i, a)

fromPair :: (EntityId, a) -> Tagged a
fromPair (i, a) = Tagged i a

class Typeable t => Store t where
  withId :: EntityId -> t a -> Maybe a
  mergeComponents :: [ Tagged a ] -> t a -> t a
  updateComponents :: [ Tagged (Maybe a) ] -> t a -> t a
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

combine5 :: [ Tagged a ] -> [ Tagged b ] -> [Tagged c ] -> [ Tagged d ] -> [ Tagged e ] -> [ Tagged (a, b, c, d, e)]
combine5 as bs cs ds es = (flatten <$>) <$> (combine2 (combine2 as bs) (combine3 cs ds es)) where
  flatten ((a, b), (c, d, e)) = (a, b, c, d, e)
