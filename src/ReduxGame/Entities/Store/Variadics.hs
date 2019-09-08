module ReduxGame.Entities.Store.Variadics where

import Data.Maybe
import Data.List

import ReduxGame.Entities.Entity
import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Store.ComponentStore

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
