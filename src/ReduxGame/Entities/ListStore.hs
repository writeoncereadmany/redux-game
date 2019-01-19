module ReduxGame.Entities.ListStore
 ( EntityId
 , ListStore
 , emptyStore
 , withId
 , replaceComponent
 , apply2
 ) where

import ReduxGame.Entities.Store

data ListStore a = ListStore [ Tagged a ]

liststore_emptyStore = ListStore []

instance Store ListStore where
  withId = liststore_withId
  replaceComponent = liststore_replaceComponent
  components (ListStore a) = a
  mergeComponents = liststore_mergeComponents
  apply2 = liststore_apply2
  emptyStore = liststore_emptyStore
  delete = liststore_delete

liststore_withId :: EntityId -> ListStore a -> Maybe a
liststore_withId entId (ListStore xs) = withId' xs where
  withId' [] = Nothing
  withId' ((Tagged entId' a) : as) =
    case compare entId entId' of
      LT -> Nothing
      EQ -> Just a
      GT -> withId' as

liststore_replaceComponent :: EntityId -> a -> ListStore a -> ListStore a
liststore_replaceComponent entId a = mergeComponents [Tagged entId a]

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


liststore_getPairs :: ListStore a -> ListStore b -> [ Tagged (a, b) ]
liststore_getPairs (ListStore as) (ListStore bs) = getPairs as bs where
  getPairs :: [Tagged a] -> [Tagged b] -> [Tagged (a, b)]
  getPairs [] _ = []
  getPairs _ [] = []
  getPairs all_as@((Tagged id_a a): rest_a) all_bs@((Tagged id_b b): rest_b) = case compare id_a id_b of
    EQ -> Tagged id_a (a, b) : getPairs rest_a rest_b
    LT -> getPairs rest_a all_bs
    GT -> getPairs all_as rest_b

liststore_apply2 :: forall a b . ((a, b) -> (a, b)) -> ListStore a -> ListStore b -> (ListStore a, ListStore b)
liststore_apply2 f (ListStore as) (ListStore bs) = let (as', bs') = apply2' as bs in (ListStore as', ListStore bs') where
  apply2' :: [ Tagged a ] -> [ Tagged b] -> ([ Tagged a ], [ Tagged b ])
  apply2' [] bs = ([], bs)
  apply2' as [] = (as, [])
  apply2' as@(firsta@(Tagged id_a a):resta) bs@(firstb@(Tagged id_b b):restb) = case compare id_a id_b of
    LT -> let (as', bs') = apply2' resta bs
           in (firsta:as', bs')
    GT -> let (as', bs') = apply2' as restb
           in (as', firstb:bs')
    EQ -> let (a', b') = f (a, b)
              (as', bs') = apply2' resta restb
           in ((Tagged id_a a'):as', (Tagged id_b b'):bs')

liststore_delete :: EntityId -> ListStore a -> ListStore a
liststore_delete entId (ListStore as) = ListStore (filter (\(Tagged entId' a) -> entId /= entId') as)
