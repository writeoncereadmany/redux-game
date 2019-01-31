module ReduxGame.Entities.Store.Variadics where

import ReduxGame.Entities.Entity
import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Store.ComponentStore

data Only a = Only a

unOnly :: Only a -> a
unOnly (Only a) = a

class Extractable a where
  extract :: forall s . Store s => ComponentStore s -> [ Tagged a ]

instance Extractable a => Extractable (Tagged a) where
  extract store = nestTags <$> extract store where
    nestTags (Tagged entId a) = Tagged entId (Tagged entId a)
  
instance Component a => Extractable (Only a) where
  extract store = fmap Only <$> storeOf store

instance (Component a, Component b) => Extractable (a, b) where
  extract store = combine2 (storeOf store) (storeOf store)

instance (Component a, Component b, Component c) => Extractable (a, b, c) where
  extract store = combine3 (storeOf store) (storeOf store) (storeOf store)

class Updatable a where
  update :: forall s . Store s => [ Tagged a ] -> ComponentStore s -> ComponentStore s

instance Updatable () where
  update _ cs = cs

instance Component a => Updatable (Only a) where
  update xs = merge (fmap unOnly <$> xs)

instance (Component a, Component b) => Updatable (a, b) where
  update xs = merge (fmap fst <$> xs)
            . merge (fmap snd <$> xs)

foldWithTags :: (Extractable a, Store s)
             => (a -> b)
             -> ComponentStore s
             -> [ Tagged b ]
foldWithTags f cs = fmap f <$> extract cs

foldStore :: (Extractable a, Store s)
          => (a -> b)
          -> ComponentStore s
          -> [ b ]
foldStore f cs = content <$> foldWithTags f cs

apply :: (Extractable a, Updatable b, Store s )
      => (a -> b)
      -> ComponentStore s
      -> ComponentStore s
apply f cs = update (fmap f <$> (extract cs)) cs

applyM :: (Extractable a, Updatable b, Monad m, Store s)
       => (a -> m b)
       -> ComponentStore s
       -> m (ComponentStore s)
applyM f cs = do
  bs <- sequence $ (mapM f) <$> (extract cs)
  return $ update bs cs
