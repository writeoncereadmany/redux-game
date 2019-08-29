module ReduxGame.Entities.Store.Variadics where

import Data.Maybe
import Data.List

import ReduxGame.Entities.Entity
import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Store.ComponentStore

mapWithTags :: (Component a, Store s)
             => (a -> b)
             -> ComponentStore s
             -> [ Tagged b ]
mapWithTags f cs = fmap f <$> getAll cs

mapStore :: (Component a, Store s)
          => (a -> b)
          -> ComponentStore s
          -> [ b ]
mapStore f cs = content <$> mapWithTags f cs

foldStore :: (Component a, Store s)
             => (a -> b -> b)
             -> b
             -> ComponentStore s
             -> b
foldStore f acc cs = foldr f acc (content <$> getAll cs)

apply :: (Component a, Component b, Store s )
      => (a -> b)
      -> ComponentStore s
      -> ComponentStore s
apply f cs = setAll (fmap f <$> (getAll cs)) cs

applyM :: (Component a, Component b, Monad m, Store s)
       => (a -> m b)
       -> ComponentStore s
       -> m (ComponentStore s)
applyM f cs = do
  bs <- sequence $ (mapM f) <$> (getAll cs)
  return $ setAll bs cs
