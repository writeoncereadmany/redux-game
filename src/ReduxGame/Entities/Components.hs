module ReduxGame.Entities.Components where

import Control.Lens
import Control.Lens.Tuple

import Data.Typeable
import Data.Maybe

import ReduxGame.Entities.Component

instance Component () where
  getAll c = []
  getById e c = Nothing
  setAll xs c = c

instance Component a => Component (Tagged a) where
  getAll c = wrap <$> getAll c where
    wrap (Tagged e a) = Tagged e (Tagged e a)
  getById e c = Tagged e <$> getById e c
  setAll xs c = setAll (unwrap <$> xs) c where
    unwrap (Tagged e (Tagged _ a)) = Tagged e a

instance (Component a, Component b) => Component (a, b) where
  getAll c = catMaybes $ addB <$> getAll c where
    addB (Tagged e a) = do
      b <- getById e c
      return $ Tagged e (a, b)
  getById e c = do
    a <- getById e c
    b <- getById e c
    return (a, b)
  setAll xs c = setAll (fmap fst <$> xs) . setAll (fmap snd <$> xs) $ c

instance (Component a, Component b, Component c) => Component (a, b, c) where
  getAll s = catMaybes $ addBC <$> getAll s where
    addBC (Tagged e a) = do
      b <- getById e s
      c <- getById e s
      return $ Tagged e (a, b, c)
  getById e s = do
    a <- getById e s
    b <- getById e s
    c <- getById e s
    return (a, b, c)
  setAll xs s = setAll (fmap (^. _1) <$> xs)
              . setAll (fmap (^. _2) <$> xs)
              . setAll (fmap (^. _3) <$> xs)
              $ s

instance (Component a, Component b, Component c, Component d) => Component (a, b, c, d) where
  getAll s = catMaybes $ addBCD <$> getAll s where
    addBCD (Tagged e a) = do
      b <- getById e s
      c <- getById e s
      d <- getById e s
      return $ Tagged e (a, b, c, d)
  getById e s = do
    a <- getById e s
    b <- getById e s
    c <- getById e s
    d <- getById e s
    return (a, b, c, d)
  setAll xs s = setAll (fmap (^. _1) <$> xs)
              . setAll (fmap (^. _2) <$> xs)
              . setAll (fmap (^. _3) <$> xs)
              . setAll (fmap (^. _4) <$> xs)
              $ s

instance (Component a, Component b, Component c, Component d, Component e) => Component (a, b, c, d, e) where
  getAll s = catMaybes $ addBCDE <$> getAll s where
    addBCDE (Tagged entId a) = do
      b <- getById entId s
      c <- getById entId s
      d <- getById entId s
      e <- getById entId s
      return $ Tagged entId (a, b, c, d, e)
  getById entId s = do
    a <- getById entId s
    b <- getById entId s
    c <- getById entId s
    d <- getById entId s
    e <- getById entId s
    return (a, b, c, d, e)
  setAll xs s = setAll (fmap (^. _1) <$> xs)
              . setAll (fmap (^. _2) <$> xs)
              . setAll (fmap (^. _3) <$> xs)
              . setAll (fmap (^. _4) <$> xs)
              . setAll (fmap (^. _5) <$> xs)
              $ s

instance (Component a) => Component (Maybe a) where
  getAll _ = []  -- ignoring for now
  getById e c = Just $ getById e c
  setAll = updateComponents

data Not a = Not

instance (Component a) => Component (Not a) where
  getAll _ = []
  getById e c = case (getById e c :: Maybe a) of
    (Just _) -> Nothing
    Nothing  -> Just Not
  setAll xs c = updateComponents (fmap (const (Nothing :: Maybe a)) <$> xs) c
