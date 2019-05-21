module ReduxGame.Entities.Store.Variadics where

import Data.Maybe
import Data.List

import ReduxGame.Entities.Entity
import ReduxGame.Entities.Store.Store
import ReduxGame.Entities.Store.ComponentStore

data Only a = Only a

unOnly :: Only a -> a
unOnly (Only a) = a

class Extractable a where
  extract :: forall s . Store s => ComponentStore s -> [ Tagged a ]
  extractWithId :: forall s . Store s => EntityId -> ComponentStore s -> Maybe a
  extractWithId entId cs = content <$> find ((entId ==) . idOf) (extract cs)

instance Extractable a => Extractable (Tagged a) where
  extract store = nestTags <$> extract store where
    nestTags (Tagged entId a) = Tagged entId (Tagged entId a)
  extractWithId entId store = Tagged entId <$> extractWithId entId store

instance Component a => Extractable (Only a) where
  extract store = fmap Only <$> storeOf store
  extractWithId entId store = Only <$> withId entId (storeOf' store)

instance (Component a, Component b) => Extractable (a, b) where
  extract store = combine2 (storeOf store) (storeOf store)
  extractWithId entId store = do
    a <- withId entId (storeOf' store)
    b <- withId entId (storeOf' store)
    return (a, b)

instance (Component a, Component b, Component c) => Extractable (a, b, c) where
  extract store = combine3 (storeOf store) (storeOf store) (storeOf store)
  extractWithId entId store = do
    a <- withId entId (storeOf' store)
    b <- withId entId (storeOf' store)
    c <- withId entId (storeOf' store)
    return (a, b, c)

instance (Component a, Component b, Component c, Component d) => Extractable (a, b, c, d) where
  extract store = combine4 (storeOf store) (storeOf store) (storeOf store) (storeOf store)
  extractWithId entId store = do
    a <- withId entId (storeOf' store)
    b <- withId entId (storeOf' store)
    c <- withId entId (storeOf' store)
    d <- withId entId (storeOf' store)
    return (a, b, c, d)

class Persistable a where
  persist :: forall s . Store s => [ Tagged a ] -> ComponentStore s -> ComponentStore s
  persistWithId :: forall s . Store s => EntityId -> a -> ComponentStore s -> ComponentStore s
  persistWithId entId a = persist [Tagged entId a]

instance Persistable () where
  persist _ cs = cs

instance Component a => Persistable (Only a) where
  persist xs = merge (fmap unOnly <$> xs)

instance (Component a, Component b) => Persistable (a, b) where
  persist xs = merge (fmap fst <$> xs)
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

actuallyFold :: (Extractable a, Store s)
             => (a -> b -> b)
             -> b
             -> ComponentStore s
             -> b
actuallyFold f acc cs = foldr f acc (content <$> extract cs)

apply :: (Extractable a, Persistable b, Store s )
      => (a -> b)
      -> ComponentStore s
      -> ComponentStore s
apply f cs = persist (fmap f <$> (extract cs)) cs

applyM :: (Extractable a, Persistable b, Monad m, Store s)
       => (a -> m b)
       -> ComponentStore s
       -> m (ComponentStore s)
applyM f cs = do
  bs <- sequence $ (mapM f) <$> (extract cs)
  return $ persist bs cs

extract_2r1d f cs = catMaybes $ build <$> storeOf cs where
  build (Tagged entId a) = do
    b <- maybeGetComponent entId cs
    let c = getComponent entId cs
    return $ Tagged entId $ f a b c

extractWithId_2r1d f entId cs = do
  a <- maybeGetComponent entId cs
  b <- maybeGetComponent entId cs
  let c = getComponent entId cs
  return $ f a b c

extract_2r2d f cs = catMaybes $ build <$> storeOf cs where
  build (Tagged entId a) = do
    b <- maybeGetComponent entId cs
    let c = getComponent entId cs
    let d = getComponent entId cs
    return $ Tagged entId $ f a b c d

extractWithId_2r2d f entId cs = do
  a <- maybeGetComponent entId cs
  b <- maybeGetComponent entId cs
  let c = getComponent entId cs
  let d = getComponent entId cs
  return $ f a b c d


extract_1r2d f cs = build <$> storeOf cs where
  build (Tagged entId a) = Tagged entId $ f a (getComponent entId cs) (getComponent entId cs)

extractWithId_1r2d f entId cs = do
  a <- maybeGetComponent entId cs
  let b = getComponent entId cs
  let c = getComponent entId cs
  return $ f a b c
