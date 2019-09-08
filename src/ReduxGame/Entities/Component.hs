module ReduxGame.Entities.Component where

import Data.Maybe
import Data.Typeable
import Data.Maybe

type EntityId = Integer

data Tagged a = Tagged EntityId a

instance Functor Tagged where
  fmap f (Tagged entId a) = Tagged entId (f a)

instance Foldable Tagged where
  foldMap f (Tagged entId a) = f a

instance Traversable Tagged where
  traverse f (Tagged entId a) = pure (Tagged entId) <*> f a

content :: Tagged a -> a
content (Tagged _ a) = a


class Components c where
  allComponents :: Component a => c -> [ Tagged a ]
  componentById :: Component a => EntityId -> c -> Maybe a
  updateComponents :: Component a => [ Tagged (Maybe a) ] -> c -> c

class Typeable a => Component a where
  getAll :: Components c => c -> [ Tagged a ]
  getAll = allComponents
  getById :: Components c => EntityId -> c -> Maybe a
  getById = componentById
  setAll :: Components c => [ Tagged a ] -> c -> c
  setAll xs c = updateComponents (fmap Just <$> xs) c

mapWithTags :: (Component a, Components c) => (a -> b) -> c -> [ Tagged b ]
mapWithTags f cs = fmap f <$> getAll cs

mapStore :: (Component a, Components c) => (a -> b) -> c -> [ b ]
mapStore f cs = content <$> mapWithTags f cs

foldStore :: (Component a, Components c) => (a -> b -> b) -> b -> c -> b
foldStore f acc cs = foldr f acc (content <$> getAll cs)

apply :: (Component a, Component b, Components c) => (a -> b) -> c -> c
apply f cs = setAll (fmap f <$> (getAll cs)) cs

applyM :: (Component a, Component b, Monad m, Components c)
       => (a -> m b) -> c -> m c
applyM f cs = do
  bs <- sequence $ (mapM f) <$> (getAll cs)
  return $ setAll bs cs
