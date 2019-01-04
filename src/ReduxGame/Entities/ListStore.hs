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

emptyStore = ListStore []

withId :: EntityId -> ListStore a -> Maybe a
withId entId (ListStore xs) = withId' xs where
  withId' [] = Nothing
  withId' ((Tagged entId' a) : as) =
    case compare entId entId' of
      LT -> Nothing
      EQ -> Just a
      GT -> withId' as

replaceComponent :: EntityId -> a -> ListStore a -> ListStore a
replaceComponent entId a (ListStore xs) = ListStore $ replaceComponent' xs where
  replaceComponent' [] = [ Tagged entId a ]
  replaceComponent' cs@(c@(Tagged entId' _) : restc) =
    case compare entId entId' of
      LT -> (Tagged entId a) : cs
      EQ -> (Tagged entId a) : restc
      GT -> c : replaceComponent' restc

apply2 :: forall a b . ((a, b) -> (a, b)) -> ListStore a -> ListStore b -> (ListStore a, ListStore b)
apply2 f (ListStore as) (ListStore bs) = let (as', bs') = apply2' as bs in (ListStore as', ListStore bs') where
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
