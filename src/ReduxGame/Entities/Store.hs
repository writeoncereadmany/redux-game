module ReduxGame.Entities.Store
 ( EntityId
 , Store
 , emptyStore
 , withId
 , replaceComponent
 , apply2''
 ) where

type EntityId = Integer

data Tagged a = Tagged EntityId a

data Store a = Store [ Tagged a ]

emptyStore = Store []

withId :: EntityId -> Store a -> Maybe a
withId entId (Store xs) = withId' xs where
  withId' [] = Nothing
  withId' ((Tagged entId' a) : as) =
    case compare entId entId' of
      LT -> Nothing
      EQ -> Just a
      GT -> withId' as

replaceComponent :: EntityId -> a -> Store a -> Store a
replaceComponent entId a (Store xs) = Store $ replaceComponent' xs where
  replaceComponent' [] = [ Tagged entId a ]
  replaceComponent' cs@(c@(Tagged entId' _) : restc) =
    case compare entId entId' of
      LT -> (Tagged entId a) : cs
      EQ -> (Tagged entId a) : restc
      GT -> c : replaceComponent' restc

apply2'' :: forall a b . ((a, b) -> (a, b)) -> Store a -> Store b -> (Store a, Store b)
apply2'' f (Store as) (Store bs) = let (as', bs') = apply2' as bs in (Store as', Store bs') where
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
