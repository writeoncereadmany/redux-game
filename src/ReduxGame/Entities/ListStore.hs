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
liststore_replaceComponent entId a (ListStore xs) = ListStore $ replaceComponent' xs where
  replaceComponent' [] = [ Tagged entId a ]
  replaceComponent' cs@(c@(Tagged entId' _) : restc) =
    case compare entId entId' of
      LT -> (Tagged entId a) : cs
      EQ -> (Tagged entId a) : restc
      GT -> c : replaceComponent' restc

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
