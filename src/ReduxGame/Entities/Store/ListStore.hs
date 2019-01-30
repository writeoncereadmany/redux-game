module ReduxGame.Entities.Store.ListStore (ListStore) where

import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Entity

data ListStore a = ListStore [ Tagged a ]

instance Store ListStore where
  withId = liststore_withId
  components (ListStore a) = a
  mergeComponents = liststore_mergeComponents
  emptyStore = ListStore []
  delete = liststore_delete

liststore_withId :: EntityId -> ListStore a -> Maybe a
liststore_withId entId (ListStore xs) = withId' xs where
  withId' [] = Nothing
  withId' ((Tagged entId' a) : as) =
    case compare entId entId' of
      LT -> Nothing
      EQ -> Just a
      GT -> withId' as

liststore_mergeComponents :: [ Tagged a ] -> ListStore a -> ListStore a
liststore_mergeComponents new (ListStore old) = ListStore $ mergeComponents' new old where
  mergeComponents' :: [Tagged a] -> [Tagged a] -> [Tagged a]
  mergeComponents' new [] = new
  mergeComponents' [] old = old
  mergeComponents' news@(new@(Tagged new_id new_c) : rest_news) olds@(old@(Tagged old_id old_c) : rest_olds) =
    case compare new_id old_id of
      GT -> old : mergeComponents' news rest_olds
      EQ -> new : mergeComponents' rest_news rest_olds
      LT -> new : mergeComponents' rest_news olds

liststore_delete :: EntityId -> ListStore a -> ListStore a
liststore_delete entId (ListStore as) = ListStore (filter (\(Tagged entId' a) -> entId /= entId') as)
